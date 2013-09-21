{-# LANGUAGE GeneralizedNewtypeDeriving, DefaultSignatures, ScopedTypeVariables, NamedFieldPuns, OverloadedStrings, MultiParamTypeClasses, TemplateHaskell, FlexibleContexts, TypeFamilies, StandaloneDeriving, RecordWildCards #-}
module System.Log.SLog
    ( module System.Log.SLog.Format
    , Severity(..)
    , MonadSLog(..)
    , defaultLogFormat
    , logD
    , logS
    , logI
    , logW
    , logE
    , Logger(..)
    , Filter
    , anySev
    , LogLine(..)
    , LogConfig(..)
    , defaultLogConfig
    , SLogT
    , SLog
    , runSLogT
    , simpleLog
    , forkSLog
    , FlushKey
    , waitFlush
    )
where

import System.Console.ANSI
import System.IO
import Control.Monad.Reader
import Data.Time.LocalTime
import Data.List
import Data.Semigroup
import Control.Applicative
import Control.Monad.State
import Control.Monad.Error
import Control.Concurrent.STM
import Control.Concurrent
import Control.Concurrent.ForkableT
import Control.Concurrent.ForkableT.Instances()
import System.Directory
import qualified Data.Map as Map
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Monad.Cont
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Prelude hiding (log)

import System.Log.SLog.Format


data Severity
    = DEBUG | INFO | SUCCESS | WARNING | ERROR
      deriving (Show, Eq, Ord)

-- SGR attributes


class (MonadIO m) => MonadSLog m where
    log :: Severity -> T.Text -> m ()
    default log :: (MonadTrans t, MonadSLog m) => Severity -> T.Text -> t m ()
    log sev = lift . log sev


instance (MonadSLog m) => MonadSLog (StateT s m)
instance (MonadSLog m) => MonadSLog (ReaderT s m)
instance (MonadSLog m, Error e) => MonadSLog (ErrorT e m)

defaultLogFormat :: LogFormat
defaultLogFormat = $(format "%d(%F %T) | %s | [%n] %m\n")

logD :: MonadSLog m => T.Text -> m ()
logD = log DEBUG

logS :: MonadSLog m => T.Text -> m ()
logS = log SUCCESS

logI :: MonadSLog m => T.Text -> m ()
logI = log INFO

logW :: MonadSLog m => T.Text -> m ()
logW = log WARNING

logE :: MonadSLog m => T.Text -> m ()
logE = log ERROR


sgr :: Severity -> [SGR]
sgr DEBUG = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]
sgr INFO = [SetColor Foreground Vivid White]
sgr SUCCESS = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green]
sgr WARNING = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
sgr ERROR = [SetConsoleIntensity BoldIntensity, SetBlinkSpeed SlowBlink, SetColor Foreground Vivid Red]

data Sync
    = Sync | Async
  deriving (Eq)

data Logger
    = FileLogger Sync FilePath
    | StdoutLogger Sync
    | StderrLogger Sync
    | TChanLogger (TChan LogLine)
      deriving (Eq)

type Filter = Severity -> Bool

anySev :: Filter
anySev = const True

data LogLine
    = LogLine { logSeverity :: Severity
              , logMessage :: T.Text
              , logTimestamp :: ZonedTime
              , logThread :: T.Text
              }

data LogConfig
    = LogConfig { ansiColours :: Bool
                , loggers :: [(Filter, Logger)]
                }

defaultLogConfig :: FilePath -> LogConfig
defaultLogConfig fName
    = LogConfig { ansiColours = True
                , loggers = [ (anySev, StdoutLogger Sync)
                            , (anySev, FileLogger Async fName)
                            ]
                }

-- data LoggerInternal
--     = AsyncFileLoggerInternal (TChan T.Text)
--     | SyncFileLoggerInternal Handle (MVar ())
--     | StdoutLoggerInternal (MVar ())
--     | StderrLoggerInternal (MVar ())
--     | TChanLoggerInternal (TChan LogLine)

data LoggerInternal
    = SyncLoggerInternal Handle (MVar ())
    | AsyncLoggerInternal (TChan T.Text)
    | TChanLoggerInternal (TChan LogLine)

withSgr :: [SGR] -> T.Text -> T.Text
withSgr sg s = T.concat [T.pack $ setSGRCode sg, s, T.pack $ setSGRCode []]

data SLogEnv
    = SLogEnv { threadName :: T.Text
              , loggerInternals :: [(Filter, LoggerInternal)]
              , logColours :: Bool
              , logFormat :: LogFormat
              }

newtype SLogT m a
    = SLogT { unSLogT :: ReaderT SLogEnv (ResourceT m) a }
      deriving ( Functor, Monad, MonadIO, Applicative
               , MonadThrow, MonadResource, MonadReader SLogEnv)
deriving instance (MonadBase IO m) => MonadBase IO (SLogT m)

-- SLogT n b -> n b

-- m (StT Resou) -> m (StT SLogT)

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

type SLog = SLogT IO

newtype FlushKey = FlushKey (TVar Bool)

waitFlush :: FlushKey -> IO ()
waitFlush (FlushKey tvar)
    = atomically $ do
        b <- readTVar tvar
        when (not b) retry

runSLogT :: (MonadIO m, MonadThrow m, MonadUnsafeIO m, Applicative m, MonadBaseControl IO m) => LogConfig -> LogFormat -> T.Text -> SLogT m a -> m (a, FlushKey)
runSLogT LogConfig { .. } lf tName (SLogT r)
    = runResourceT $ do
        (_, tvar) <- allocate (newTVarIO False) (\t -> atomically $ writeTVar t True)
        internals <- initLoggers loggers
        a <- runReaderT r SLogEnv{ threadName = tName
                                 , loggerInternals = internals
                                 , logColours = ansiColours
                                 , logFormat = lf }
        return (a, FlushKey tvar)

simpleLog :: (MonadIO m, MonadUnsafeIO m, MonadThrow m, Applicative m, MonadBaseControl IO m) => FilePath -> SLogT m a -> m a
simpleLog fName s = do
  tName <- return . T.pack . show =<< liftIO myThreadId
  (a, fkey) <- runSLogT (defaultLogConfig fName) defaultLogFormat tName s
  liftIO $ waitFlush fkey
  return a

initLoggers :: (MonadIO m, MonadThrow m, MonadUnsafeIO m, Applicative m) => [(Filter, Logger)] -> ResourceT m [(Filter, LoggerInternal)]
initLoggers fls = do
  InitState{..} <- liftIO $ aggregateLoggers fls

  let stdHandle (Just ini) h = do
        _ <- register (hFlush h)
        return [(h, ini)]
      stdHandle Nothing _ = return []

      createHandle (fname, ini) = do
        (_, h) <- allocate
                  (openFile fname AppendMode)
                  (\h -> hFlush h >> hClose h)
        return (h, ini)

  sout <- stdHandle stdoutInit stdout
  serr <- stdHandle stderrInit stderr
  files <- mapM createHandle $ Map.toList fileInitMap

  let toInternal (h, InitSync f) = do
        lock <- liftIO $ newMVar ()
        return $ [(f, SyncLoggerInternal h lock)]
      toInternal (h, InitAsync f) = do
        tchan <- liftIO newTChanIO
        _ <- forkCleanUp $ lift . asyncLogger Nothing h tchan
        return $ [(f, AsyncLoggerInternal tchan)]
      toInternal (h, Both fs fa) = do
        lock <- liftIO $ newMVar ()
        tchan <- liftIO newTChanIO
        _ <- forkCleanUp $ lift . asyncLogger (Just lock) h tchan
        return $ [(fs, SyncLoggerInternal h lock), (fa, AsyncLoggerInternal tchan)]

      toInternalTChan (f, tchan) = (f, TChanLoggerInternal tchan)
  
  nonTChan <- join <$> mapM toInternal (sout ++ serr ++ files)

  return $ nonTChan ++ map toInternalTChan tchanInit


data InitLogger
    = InitSync Filter -- Handle (MVar ())
    | InitAsync Filter -- (TChan T.Text) Handle (MVar ())
    | Both Filter Filter -- Handle (MVar ()) Filter (TChan T.Text)

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

-- makes sure file exists by creating it if it doesn't, then returns canonicalizePath
canonExist :: String -> IO String
canonExist f = appendFile f "" >> canonicalizePath f

-- Forks a thread that will get an exit signal through a TVar when the ResourceT is run
forkCleanUp :: (MonadIO m, MonadThrow m, MonadUnsafeIO m, Applicative m) =>
               (TVar Bool -> ResIO ()) -> ResourceT m ThreadId
forkCleanUp io = do
  exitSignal <- liftIO $ newTVarIO False
  stst <- liftWith $ \unliftRes -> unliftRes $ do
    (_, st) <- allocate
      (unliftRes . resourceForkIO $ io exitSignal)
      (\_ -> atomically $ writeTVar exitSignal True)
    return st
  restoreT . return =<< restoreT (return stst)

data InitState = InitState { fileInitMap :: Map.Map FilePath InitLogger
                           , stdoutInit :: Maybe InitLogger
                           , stderrInit :: Maybe InitLogger
                           , tchanInit :: [(Filter, TChan LogLine)]
                           }

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
      initLogger f (TChanLogger tchan) = do
        modify $ \s@InitState{..} -> s { tchanInit = (f, tchan) : tchanInit }

asyncLogger :: Maybe (MVar ()) -> Handle -> TChan T.Text -> TVar Bool -> IO ()
asyncLogger mlock h tchan exitSignal = flip runContT return $ do
  callCC $ \exit -> forever $ do
    m <- liftIO . atomically $
           (Just <$> readTChan tchan)
             `orElse`
           (readTVar exitSignal >>= check >> return Nothing)
    case m of
      Just str -> liftIO $ case mlock of
                    Nothing -> T.hPutStr h str
                    Just lock -> withMVar lock $ \_ -> T.hPutStr h str
      Nothing -> exit ()

-- the first string is noncoloured, the second may be coloured depending on config
logger :: LoggerInternal -> LogLine -> T.Text -> T.Text -> IO ()
logger (AsyncLoggerInternal tchan) _ s _ = atomically $ writeTChan tchan s
logger (SyncLoggerInternal h lock) _ s _ = withMVar lock $ \_ -> T.hPutStr h s >> hFlush h
logger (TChanLoggerInternal tchan) l _ _ = atomically $ writeTChan tchan l

formatLine :: Bool -> LogFormat -> LogLine -> T.Text
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
    fork (SLogT (ReaderT f)) = SLogT . ReaderT $ \env -> do
                                 fork $ do
                                   tid <- liftIO myThreadId
                                   f env { threadName = T.pack $ show tid }

forkSLog :: (MonadBaseControl IO m, MonadIO m) => T.Text -> SLogT m () -> SLogT m ThreadId
forkSLog tname (SLogT m) = SLogT . local (\e -> e { threadName = tname }) $ fork m

padS :: Int -> T.Text -> T.Text
padS n t = t `T.append` T.replicate (T.length t - n) " "

instance (MonadIO m, Functor m) => MonadSLog (SLogT m) where
    log sev s = do
      SLogEnv{ threadName
             , loggerInternals
             , logColours
             , logFormat } <- SLogT ask
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
