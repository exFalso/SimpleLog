{-# LANGUAGE GeneralizedNewtypeDeriving, DefaultSignatures, ScopedTypeVariables, NamedFieldPuns, OverloadedStrings, MultiParamTypeClasses, TemplateHaskell, FlexibleContexts, TypeFamilies, StandaloneDeriving, RecordWildCards, RankNTypes, Trustworthy, UndecidableInstances #-}
{-|
  SimpleLog is a library for convenient and configurable logging. It uses the usual monad transformer + associated class design: 'SLogT' and 'MonadSLog'.

  Example usage:

  @
import System.Log.SLog

main = simpleLog \"Example.log\" $ do
    logD \"Some debugging information\"
    logI \"This is some other information\"
    logW \"Something bad is about to happen\"
    logE \"Something bad happened\"
  @

  The above sample code when run will produce output similar to this:

  @
2013-10-02 14:17:40 | INFO    | [ThreadId 58] This is some other information
2013-10-02 14:17:40 | WARNING | [ThreadId 58] Something bad is about to happen
2013-10-02 14:17:40 | ERROR   | [ThreadId 58] Something bad happened
  @

  Note how the debug line is not displayed. This is because the default configuration ('defaultLogConfig') only logs to stdout when the severity is >= 'INFO'.
  The above code will also append the log lines to the file @\"Example.log\"@, including the debug line.

  The following example shows how one can fine tune SimpleLog as well as how to fork other logging threads.

  @
\-- Our log configuration specifies that no ANSI colouring should be used, all log lines
\-- should be written to the TChan, and >= INFO severity lines should be written to the
\-- stdout synchronously.
logConfig :: TChan LogLine -> LogConfig
logConfig tchan
    = LogConfig { ansiColours = False
                , loggers = [ (anySev, TChanLogger tchan)
                            , ((>= INFO), StdoutLogger Sync)
                            ] }


\-- Our custom logging format
logFormat :: Format
logFormat = $(format \"%d(%T) (%s) %t: %m\")

\-- The main thread will fork a child thread, then wait until everything is flushed, then
\-- count how many messages have been written in total to the TChan (which will be all
\-- messages as our filter passes through everything)
main :: IO ()
main = do
  tchan \<- newTChanIO
  (_, fkey) \<- runSLogT (logConfig tchan) logFormat \"main\" $ do
    logS \"Main thread started successfully\"
    logD \"This will not appear on stdout\"
    _ \<- forkSLog \"child\" $ do
      logS \"I am the child\"
      liftIO $ threadDelay 5000000
      logW \"CHILD SHUTTING DOWN\"
    logI \"Exiting main thread\"
  waitFlush fkey
  c \<- countTChan tchan
  putStrLn $ show c ++ \" messages have been logged in total\"

\-- Counts the number of elements in the TChan (and pops them all)
countTChan :: TChan a -> IO Int
countTChan tchan = do
  let count = do
        em \<- isEmptyTChan tchan
        if em then return 0
        else readTChan tchan >> (1 +) \<$> count
  atomically count
  @

  The above code when run will produce something like this:

  @
17:35:15 (SUCCESS) main: Main thread started successfully
17:35:15 (SUCCESS) child: I am the child, waiting for 5 seconds...
17:35:15 (INFO   ) main: Exiting main thread
17:35:20 (WARNING) child: CHILD SHUTTING DOWN
5 messages have been logged in total
  @

-}
module System.Log.SLog
    (
    -- * SLogT
      SLogT
    , SLog

    -- ** Running SLogT
    , runSLogT
    , simpleLog

    -- * FlushKey
    , FlushKey
    , waitFlush

    -- * MonadSLog
    , MonadSLog(..)

    -- ** Convenience log functions
    , logD
    , logI
    , logS
    , logW
    , logE

    -- * Loggers
    , Logger(..)
    , Sync(..)
    , LogLine(..)

    -- * Filters
    , Severity(..)
    , Filter
    , anySev

    -- * Configuration
    , LogConfig(..)
    , defaultLogConfig

    -- * Format
    , module System.Log.SLog.Format
    , defaultLogFormat

    -- * Utility functions
    , forkSLog
    , formatLine
    , unsafeUnliftSLogT
    )
where

import System.Log.SLog.Format

import Prelude hiding (log)
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Cont
import Control.Monad.Base
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Control
import Data.Semigroup
import qualified Data.Map as Map
import Data.Time.LocalTime
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import System.Console.ANSI
import System.Directory
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.ForkableT.Instances

-- | The type of severities with increasing importance
data Severity
    = DEBUG | INFO | SUCCESS | WARNING | ERROR
      deriving (Show, Read, Eq, Ord)

-- | The class of monads that can perform logging
class (MonadIO m) => MonadSLog m where
    -- | 'log' logs the specified 'T.Text' with the specified 'Severity'
    log :: Severity -> T.Text -> m ()
    -- | The default instance simply lifts through a monad transformer
    default log :: (MonadTrans t, MonadSLog m) => Severity -> T.Text -> t m ()
    log sev = lift . log sev

instance (MonadSLog m) => MonadSLog (StateT s m)
instance (MonadSLog m) => MonadSLog (ReaderT s m)
instance (MonadSLog m, Error e) => MonadSLog (ErrorT e m)
instance (MonadSLog m) => MonadSLog (ContT r m)
instance (MonadSLog m) => MonadSLog (ResourceT m)

-- | The default log format, which currently is @$(format \"%d(%F %T) | %s | [%t] %m\")@. See "System.Log.SLog.Format" for more details on format strings.
defaultLogFormat :: Format
defaultLogFormat = $(format "%d(%F %T) | %s | [%t] %m")

-- | Log a 'DEBUG' message
logD :: MonadSLog m => String -> m ()
logD = log DEBUG . T.pack

-- | Log a 'SUCCESS' message
logS :: MonadSLog m => String -> m ()
logS = log SUCCESS . T.pack

-- | Log an 'INFO' message
logI :: MonadSLog m => String -> m ()
logI = log INFO . T.pack

-- | Log a 'WARNING' message
logW :: MonadSLog m => String -> m ()
logW = log WARNING . T.pack

-- | Log an 'ERROR' message
logE :: MonadSLog m => String -> m ()
logE = log ERROR . T.pack

-- | SGR code for severity colours
sgr :: Severity -> [SGR]
sgr DEBUG = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]
sgr INFO = [SetColor Foreground Vivid White]
sgr SUCCESS = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green]
sgr WARNING = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
sgr ERROR = [SetConsoleIntensity BoldIntensity, SetBlinkSpeed SlowBlink, SetColor Foreground Vivid Red]

-- | Wrap a piece of text in some SGR configuration
withSgr :: [SGR] -> T.Text -> T.Text
withSgr sg s = T.concat [T.pack $ setSGRCode sg, s, T.pack $ setSGRCode []]

-- | 'Sync' is a type to specify whether a logger should log synchronously or asynchronously.
-- Syncronous logging means that the logging thread will block until the message has been written and flushed to the sink.
-- Asynchronous logging means that the logging thread will write to a work queue and move on. The work queue will be read by a dedicated thread that is forked for each sink.
data Sync
    = Sync | Async
  deriving (Eq)

-- | The 'Logger' type specifies the types of sinks we can log to.
data Logger
    -- | 'FileLogger' specifies a file to be logged in.
    -- Note that symbolic links will be resolved using 'canonicalizePath' when deciding whether two 'FileLogger's point to the same file.
    = FileLogger Sync FilePath
    -- | 'StdoutLogger' logs to the stdout
    | StdoutLogger Sync
    -- | 'StderrLogger' logs to the stderr
    | StderrLogger Sync
    -- | 'TChanLogger' logs to a specified 'TChan'.
    -- Note that 'LogLine's are written instead of the final formatted text. If you wish to use the final text use 'formatLine'.
    | TChanLogger (TChan LogLine)
      deriving (Eq)

-- | 'Filter' is the type of logging filters. 'Filter's may only depend on the 'Severity'.
type Filter = Severity -> Bool

-- | 'anySev' allows all lines to be logged.
anySev :: Filter
anySev = const True

-- | 'LogLine' is a log message together with the severity, time of logging and the logging thread's name.
data LogLine
    = LogLine { logSeverity :: Severity
              , logMessage :: T.Text
              , logTimestamp :: ZonedTime
              , logThread :: T.Text
              }

-- | 'LogConfig' is the configuration of 'SLogT'
data LogConfig
    = LogConfig {
        -- | Specifies whether ANSI colouring should be used when logging to stdout/stderr
        ansiColours :: Bool
        -- | The list of loggers together with the associated filters
      , loggers :: [(Filter, Logger)]
      }

-- | 'defaultLogConfig' is the default log configuration.
-- It writes all non-DEBUG messages to the stdout synchronously and all messages to a specified file asynchronously.
defaultLogConfig :: FilePath -> LogConfig
defaultLogConfig fName
    = LogConfig { ansiColours = True
                , loggers = [ ((>= INFO), StdoutLogger Sync)
                            , (anySev, FileLogger Async fName)
                            ]
                }

-- | The internal representation of Loggers with open handles, locks and 'TChan's.
data LoggerInternal
    = SyncLoggerInternal Handle (MVar ()) Bool
    | AsyncLoggerInternal (TChan T.Text) Bool
    | TChanLoggerInternal (TChan LogLine)

-- | The environment of 'SLogT'
data SLogEnv
    = SLogEnv {
        -- | The current thread's name
        threadName :: T.Text
        -- | The list of internal loggers together with associated filters
      , loggerInternals :: [(Filter, LoggerInternal)]
        -- | Same as the user-specified 'ansiColours'
      , logColours :: Bool
        -- | The 'Format' of logging
      , logFormat :: Format
      }

-- | The SLogT monad transformer is simply a 'ResourceT' with an environment
newtype SLogT m a
    = SLogT { unSLogT :: ReaderT SLogEnv (ResourceT m) a }
      deriving ( Functor, Monad, MonadIO, Applicative
               , MonadThrow )
deriving instance (MonadBase IO m) => MonadBase IO (SLogT m)
deriving instance (MonadBase IO m, MonadThrow m, MonadIO m) => MonadResource (SLogT m)

instance MonadTransControl SLogT where
    newtype StT SLogT a = StTSLogT {unStTSLogT :: StT ResourceT a}
    liftWith f = SLogT . ReaderT $ \r ->
                   liftWith $ \lres ->
                     f $ \(SLogT t) ->
                       liftM StTSLogT $ lres $ runReaderT t r
    restoreT = SLogT . lift . restoreT . liftM unStTSLogT

instance (MonadBaseControl IO m) => MonadBaseControl IO (SLogT m) where
    newtype StM (SLogT m) a = StMSLogT { unStMSLogT :: ComposeSt SLogT m a }
    liftBaseWith = defaultLiftBaseWith StMSLogT
    restoreM = defaultRestoreM unStMSLogT

instance MonadTrans SLogT where
    lift = SLogT . lift . lift

-- | This is a simple monad for the bottom of one's monad stack.
type SLog = SLogT IO

-- | A 'FlushKey' is returned when an 'SLogT' is run. You may wait on it with 'waitFlush'.
newtype FlushKey = FlushKey (TVar Bool)

-- | 'waitFlush' will only return when all resources have been released and all streams have been flushed. Note that this includes resources allocated by the user using the exposed 'MonadResource' instance.
-- 
-- All threads internally accounted for are signaled to exit (they will first finish processing of all remaining jobs) when the 'SLogT' is run, however it is the user's responsibility to shut down threads forked with 'forkSLog' or 'fork' before 'waitFlush' can return.
waitFlush :: FlushKey -> IO ()
waitFlush (FlushKey tvar)
    = atomically $ do
        b <- readTVar tvar
        unless b retry

-- Internally we use two ResourceTs. The inner one is used to keep
-- track of open files, make sure everything gets flushed and release
-- the FlushKey.
-- The outer one is used to signal completion. In particular when the
-- outer ResourceT is run all threads that are internally accounted
-- for will receive a signal to finish processing, thus running the
-- internal ResourceT (see forkCleanUp)
-- Note that this means all SLog threads forked by the user need to
-- finish before the FlushKey releases
-- | 'runSLogT' runs an 'SLogT' given a 'LogConfig', 'Format' and the current thread's name.
-- It returns a 'FlushKey' besides the usual return value.
runSLogT :: (MonadResource m, MonadBaseControl IO m) => LogConfig -> Format -> String -> SLogT m a -> m (a, FlushKey)
runSLogT LogConfig{..} lf tName (SLogT r)
    = runResourceT $ do
        (_, tvar) <- allocate (newTVarIO False) (\t -> atomically $ writeTVar t True)
        runResourceT $ do
          internals <- initLoggers loggers
          a <- lift $ runReaderT r SLogEnv{ threadName = T.pack tName
                                          , loggerInternals = internals
                                          , logColours = ansiColours
                                          , logFormat = lf }
          return (a, FlushKey tvar)

-- | 'simpleLog' uses the default configuration with the specified log file name. It also waits using 'waitFlush' until all resources have been released.
simpleLog :: (MonadResource m, MonadBaseControl IO m) => FilePath -> SLogT m a -> m a
simpleLog fName s = do
  tName <- show <$> liftIO myThreadId
  (a, fkey) <- runSLogT (defaultLogConfig fName) defaultLogFormat tName s
  liftIO $ waitFlush fkey
  return a

-- | initLoggers initialises the user specified 'Logger's and returns the internal representation of them.
-- 
-- This includes first aggregating the 'Logger's resolving any ambiguities, then opening the logging streams.
initLoggers :: (MonadResource m, Applicative m) => [(Filter, Logger)] -> ResourceT (ResourceT m) [(Filter, LoggerInternal)]
initLoggers fls = do
  InitState{..} <- liftIO $ aggregateLoggers fls

  let stdHandle (Just ini) h = do
        _ <- lift $ register (hFlush h)
        return [(h, ini, True)]
      stdHandle Nothing _ = return []

      createHandle (fname, ini) = do
        (_, h) <- allocate
                  (openFile fname AppendMode)
                  (\h -> hFlush h >> hClose h)
        return (h, ini, False)

  sout <- stdHandle stdoutInit stdout
  serr <- stdHandle stderrInit stderr
  files <- lift . mapM createHandle $ Map.toList fileInitMap

  let toInternal (h, InitSync f, c) = do
        lock <- liftIO $ newMVar ()
        return [(f, SyncLoggerInternal h lock c)]
      toInternal (h, InitAsync f, c) = do
        tchan <- liftIO newTChanIO
        _ <- forkCleanUp $ lift . asyncLogger Nothing h tchan
        return [(f, AsyncLoggerInternal tchan c)]
      toInternal (h, Both fs fa, c) = do
        lock <- liftIO $ newMVar ()
        tchan <- liftIO newTChanIO
        _ <- forkCleanUp $ lift . asyncLogger (Just lock) h tchan
        return [(fs, SyncLoggerInternal h lock c), (fa, AsyncLoggerInternal tchan c)]

      toInternalTChan (f, tchan) = (f, TChanLoggerInternal tchan)

  nonTChan <- join <$> mapM toInternal (sout ++ serr ++ files)

  return $ nonTChan ++ map toInternalTChan tchanInit

-- | An internal datatype used for aggregation of filters as well as keeping track of synchronous and asynchronous logging.
-- This means for example that it is possible to log from two threads into the same file, one synchronously, one asynchronously, using different filters.
data InitLogger
    = InitSync Filter
    | InitAsync Filter
    | Both Filter Filter

instance Semigroup InitLogger where
    InitSync f <> InitSync f' = InitSync $ liftM2 (||) f f'
    InitSync f <> InitAsync f' = Both f f'
    InitSync f <> Both f' f'' = Both (liftM2 (||) f f') f''
    InitAsync f <> InitSync f' = Both f f'
    InitAsync f <> InitAsync f' = InitAsync $ liftM2 (||) f f'
    InitAsync f <> Both f' f'' = Both f' (liftM2 (||) f f'')
    Both f' f'' <> InitSync f = Both (liftM2 (||) f f') f''
    Both f' f'' <> InitAsync f = Both f' (liftM2 (||) f f'')
    Both f f' <> Both f'' f''' = Both (liftM2 (||) f f') (liftM2 (||) f'' f''')

-- | canonExist makes sure the specified file exists by creating it if it doesn't, then returns canonicalizePath.
canonExist :: String -> IO String
canonExist f = appendFile f "" >> canonicalizePath f

-- | 'forkCleanup' forks a ResIO thread that will get an exit signal through a TVar when the outer ResourceT is run.
-- The forked off ResourceT is the inner one
forkCleanUp :: (MonadResource m) =>
               (TVar Bool -> ResIO ()) -> ResourceT (ResourceT m) ThreadId
forkCleanUp io = do
  (_, exitSignal) <- allocate (newTVarIO False) (\t -> atomically $ writeTVar t True)
  st <- lift . liftWith $ \unliftRes -> liftIO . unliftRes . fork $ io exitSignal
  lift . restoreT $ return st

-- | An internal datatype used when initialising 'Logger's. It includes all information that the user passed in with the list of loggers.
data InitState = InitState { fileInitMap :: Map.Map FilePath InitLogger
                           , stdoutInit :: Maybe InitLogger
                           , stderrInit :: Maybe InitLogger
                           , tchanInit :: [(Filter, TChan LogLine)]
                           }

-- | This method aggregates the specified loggers in a meaningful way
-- Ambiguous cases arise when several loggers specify the same file/stream
--  * First off we use canonicalizePath to resolve symlinks
--  * Second, we create a disjunction of the attached filters
--  * Lastly we keep track of synchrony: if there are two loggers
--    specifying the same file, one synchronous the other asynchronous
--    then we keep both filters and will use the a lock in each case.
aggregateLoggers :: [(Filter, Logger)] -> IO InitState
aggregateLoggers fls = execStateT
                       (mapM_ (uncurry initLogger) fls)
                       InitState { fileInitMap = Map.empty
                                 , stdoutInit = Nothing
                                 , stderrInit = Nothing
                                 , tchanInit = [] }
    where
      initLogger :: Filter -> Logger -> StateT InitState IO ()
      initLogger f (FileLogger sync fname) = do
        trueFname <- liftIO $ canonExist fname
        s@InitState{..} <- get
        let ini = case sync of ; Sync -> InitSync f ; Async -> InitAsync f
        put s { fileInitMap = Map.alter (<> Just ini) trueFname fileInitMap }
      initLogger f (StdoutLogger sync) = do
        s@InitState{..} <- get
        let ini = case sync of ; Sync -> InitSync f ; Async -> InitAsync f
        put s { stdoutInit = stdoutInit <> Just ini }
      initLogger f (StderrLogger sync) = do
        s@InitState{..} <- get
        let ini = case sync of ; Sync -> InitSync f ; Async -> InitAsync f
        put s { stderrInit = stderrInit <> Just ini }
      initLogger f (TChanLogger tchan) =
        modify $ \s@InitState{..} -> s { tchanInit = (f, tchan) : tchanInit }

-- | asyncLogger is the thread forked for each 'Async' logger.
-- It may be passed a lock to use when logging in case a 'Sync'hronous logger for the same handle also exists.
asyncLogger :: Maybe (MVar ()) -> Handle -> TChan T.Text -> TVar Bool -> IO ()
asyncLogger mlock h tchan exitSignal = flip runContT return $
  callCC $ \exit -> forever $ do
    m <- liftIO . atomically $
           (Just <$> readTChan tchan)
             `orElse`
           (readTVar exitSignal >>= check >> return Nothing)
    case m of
      Just str -> liftIO $ case mlock of
                    Nothing -> T.hPutStrLn h str
                    Just lock -> withMVar lock $ \_ -> T.hPutStrLn h str
      Nothing -> do
        liftIO $ hFlush h
        exit ()

-- | Helper method for choosing coloured vs. non-coloured strings
chs :: Bool -> a -> a -> a
chs False a _ = a
chs True  _ b = b

-- | logger performs the actual logging.
logger :: LoggerInternal -> LogLine -> T.Text -> T.Text -> IO ()
logger (AsyncLoggerInternal tchan c) _ ns s = atomically . writeTChan tchan $ chs c ns s
logger (SyncLoggerInternal h lock c) _ ns s = withMVar lock $ \_ -> do
                                                T.hPutStrLn h (chs c ns s)
                                                hFlush h
logger (TChanLoggerInternal tchan) l _ _ = atomically $ writeTChan tchan l

-- | 'formatLine' formats the given 'LogLine' using the specified 'Format'. The 'Bool'ean determines whether 'formatLine' should insert ANSI colour codes or not.
formatLine :: Bool -> Format -> LogLine -> T.Text
formatLine isColour les ll = T.concat $ map (formatLine' ll) les
    where
      formatLine' LogLine{logMessage} MessageElem    = logMessage
      formatLine' LogLine{logSeverity} SeverityElem  =
          let sev = padS 7 . T.pack $ show logSeverity
          in if isColour then withSgr (sgr logSeverity) sev else sev
      formatLine' _ (StringElem str)                 = str
      formatLine' LogLine{logTimestamp} (DateTimeElem f) = f logTimestamp
      formatLine' LogLine{logThread} ThreadElem      = logThread

instance (MonadBaseControl IO m, MonadIO m) => Forkable (SLogT m) (SLogT m) where
    fork (SLogT (ReaderT f)) = SLogT . ReaderT $ \env ->
                                 fork $ do
                                   tid <- liftIO myThreadId
                                   f env { threadName = T.pack $ show tid }

-- | 'forkSLog' forks an 'SLogT' thread with the specified thread name.
forkSLog :: (MonadBaseControl IO m, MonadIO m) => String -> SLogT m () -> SLogT m ThreadId
forkSLog tname (SLogT m) = SLogT . local (\e -> e { threadName = T.pack tname }) $ fork m

-- | helper method for padding with spaces
padS :: Int -> T.Text -> T.Text
padS n t = t `T.append` T.replicate (n - T.length t) " "

-- | 'unsafeUnliftSLog' gives you an unsafe unlift of an SLogT by assuming that any unlifted computation will finish earlier than the runSLogT of the calling thread.
-- It is unsafe because if the unlifted computation doesn't finish earlier then it may access deallocated resources.
-- This is useful when a library is implicitly forking but we still need to log in the forked threads, and we know that the child threads will finish earlier than the parent. An example is Network.WebSockets
unsafeUnliftSLogT :: forall m b. (Monad m, MonadBaseControl IO m) =>
                    ((forall a. SLogT m a -> m a) -> SLogT m b) -> SLogT m b
unsafeUnliftSLogT f = do
  env <- SLogT ask
  let unlift :: SLogT m c -> m c
      unlift s = runResourceT $ runReaderT (unSLogT s) env
  f unlift

instance (MonadIO m) => MonadSLog (SLogT m) where
    log sev s = do
      SLogEnv{..} <- SLogT ask
      liftIO $ do
        timestamp <- getZonedTime
        let logLine = LogLine { logMessage = s
                              , logSeverity = sev
                              , logTimestamp = timestamp
                              , logThread = threadName
                              }
            nonColoured = formatLine False logFormat logLine
            coloured = if logColours
                       then formatLine True logFormat logLine
                       else nonColoured
        mapM_ (\(fter, l) -> when (fter sev) $
                             logger l logLine nonColoured coloured) loggerInternals
