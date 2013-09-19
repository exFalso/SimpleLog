{-# LANGUAGE GeneralizedNewtypeDeriving, DefaultSignatures, ScopedTypeVariables, NamedFieldPuns, OverloadedStrings, MultiParamTypeClasses, TemplateHaskell, FlexibleContexts, TypeFamilies, StandaloneDeriving #-}
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
    )
where

import System.Console.ANSI
import System.IO
import Control.Monad.Reader
import Data.Time.LocalTime
import Data.List
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

import Prelude hiding (log)

import System.Log.SLog.Format


data Severity
    = DEBUG | INFO | SUCCESS | WARNING | ERROR
      deriving (Show, Eq, Ord)

-- SGR attributes


class (MonadIO m) => MonadSLog m where
    log :: Severity -> String -> m ()
    default log :: (MonadTrans t, MonadSLog m) => Severity -> String -> t m ()
    log sev = lift . log sev


instance (MonadSLog m) => MonadSLog (StateT s m)
instance (MonadSLog m) => MonadSLog (ReaderT s m)
instance (MonadSLog m, Error e) => MonadSLog (ErrorT e m)

defaultLogFormat :: LogFormat
defaultLogFormat = for $(mat "%d(%F %T) | %s | [%n] %m\n")

logD :: MonadSLog m => String -> m ()
logD = log DEBUG

logS :: MonadSLog m => String -> m ()
logS = log SUCCESS

logI :: MonadSLog m => String -> m ()
logI = log INFO

logW :: MonadSLog m => String -> m ()
logW = log WARNING

logE :: MonadSLog m => String -> m ()
logE = log ERROR


sgr :: Severity -> [SGR]
sgr DEBUG = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]
sgr INFO = [SetColor Foreground Vivid White]
sgr SUCCESS = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green]
sgr WARNING = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
sgr ERROR = [SetConsoleIntensity BoldIntensity, SetBlinkSpeed SlowBlink, SetColor Foreground Vivid Red]


data Logger
    = AsyncFileLogger String
    | SyncFileLogger String
    | StdoutLogger
    | StderrLogger
    | TChanLogger (TChan LogLine)
      deriving (Eq)

type Filter = Severity -> Bool

anySev :: Filter
anySev = const True

data LogLine
    = LogLine { logSeverity :: Severity
              , logMessage :: String
              , logTimestamp :: ZonedTime
              , logThread :: String
              }

data LogConfig
    = LogConfig { ansiColours :: Bool
                , loggers :: [(Filter, Logger)]
                }

defaultLogConfig :: String -> LogConfig
defaultLogConfig fName
    = LogConfig { ansiColours = True
                , loggers = [ (anySev, StdoutLogger)
                            , (anySev, SyncFileLogger fName)
                            ]
                }

data LoggerInternal
    = AsyncFileLoggerInternal (TChan String)
    | SyncFileLoggerInternal Handle (MVar ())
    | StdoutLoggerInternal (MVar ())
    | StderrLoggerInternal (MVar ())
    | TChanLoggerInternal (TChan LogLine)

withSgr :: [SGR] -> String -> String
withSgr sg s = setSGRCode sg ++ s ++ setSGRCode []

data SLogEnv
    = SLogEnv { threadName :: String
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

runSLogT :: (MonadIO m, MonadThrow m, MonadUnsafeIO m, Applicative m, MonadBaseControl IO m) => LogConfig -> LogFormat -> String -> SLogT m a -> m a
runSLogT LogConfig{ ansiColours
                  , loggers }
         lf tName (SLogT r)
    = runResourceT $ do
        internals <- initLoggers loggers
        runReaderT r SLogEnv{ threadName = tName
                            , loggerInternals = internals
                            , logColours = ansiColours
                            , logFormat = lf }

simpleLog :: (MonadIO m, MonadUnsafeIO m, MonadThrow m, Applicative m, MonadBaseControl IO m) => String -> SLogT m a -> m a
simpleLog fName s = do
  tName <- return . show =<< liftIO myThreadId
  runSLogT (defaultLogConfig fName) defaultLogFormat tName s

initLoggers :: (MonadIO m, MonadThrow m, MonadUnsafeIO m, Applicative m) => [(Filter, Logger)] -> ResourceT m [(Filter, LoggerInternal)]
initLoggers ls = do
  stdLock <- liftIO $ newMVar ()
  let (sout, nsout) = partition (\(_, l) -> l == StdoutLogger) ls
      (serr, nserr) = partition (\(_, l) -> l == StderrLogger) nsout
      soutInt = case sout of { [] -> id ; _ -> ((disj (map fst sout),
                                                 StdoutLoggerInternal stdLock) :) }
      serrInt = case serr of { [] -> id ; _ -> ((disj (map fst serr),
                                                 StderrLoggerInternal stdLock) :) }
      (tchanLs, ntchan) =
          partition (\(_, l) -> case l of { TChanLogger{} -> True ; _ -> False }) nserr
  ils <- map snd . Map.toList <$> execStateT (mapM initLogger ntchan) Map.empty
  let ilToFilterLogInt (InitSync f h lock) = [(f, SyncFileLoggerInternal h lock)]
      ilToFilterLogInt (InitAsync f tchan _ _) = [(f, AsyncFileLoggerInternal tchan)]
      ilToFilterLogInt (Both f h lock f' tchan) = [ (f, SyncFileLoggerInternal h lock)
                                                  , (f', AsyncFileLoggerInternal tchan) ]
      final = soutInt . serrInt $ tchanLogToInternal tchanLs ++ concatMap ilToFilterLogInt ils
  return final
    where
      disj = foldr (liftM2 (||)) (const False)
      tchanLogToInternal [] = []
      tchanLogToInternal ((f, TChanLogger tchan) : l)
          = (f, TChanLoggerInternal tchan) : tchanLogToInternal l
      tchanLogToInternal (_ : l) = tchanLogToInternal l


data InitLogger
    = InitSync Filter Handle (MVar ())
    | InitAsync Filter (TChan String) Handle (MVar ())
    | Both Filter Handle (MVar ()) Filter (TChan String)

-- makes sure file exists by creating it if it doesn't, then returns canonicalizePath
canonExist :: String -> IO String
canonExist f = appendFile f "" >> canonicalizePath f

initLogger :: (MonadIO m, MonadThrow m, MonadUnsafeIO m, Applicative m) => (Filter, Logger) -> StateT (Map.Map String InitLogger) (ResourceT m) ()
initLogger (fter, l)
    = case l of
        AsyncFileLogger f -> do
          trueF <- liftIO $ canonExist f
          m <- get
          nv <- case Map.lookup trueF m of
            Nothing -> do
              (_, h) <- allocate (openFile trueF AppendMode) $
                \h -> hFlush h >> hClose h
              tchan <- liftIO newTChanIO
              lock <- liftIO $ newMVar ()
              _ <- liftIO . forkIO $ asyncLogger h lock tchan
              return $ InitAsync fter tchan h lock
            Just (InitSync fter' h lock) -> do
              _ <- register $ hFlush h
              liftIO $ do
                tchan <- newTChanIO
                _ <- forkIO $ asyncLogger h lock tchan
                return $ Both fter' h lock fter tchan
            Just (InitAsync fter' tchan h lock) -> liftIO $ do
              return $ InitAsync (liftM2 (||) fter' fter) tchan h lock
            Just (Both fter' h lock fter'' tchan) -> liftIO $ do
              return $ Both fter' h lock (liftM2 (||) fter fter'') tchan
          modify (Map.insert trueF nv)
        SyncFileLogger f -> do
               trueF <- liftIO $ canonExist f
               m <- get
               nv <- case Map.lookup trueF m of
                       Nothing -> do
                              (_, h) <- allocate (openFile trueF AppendMode) hClose
                              lock <- liftIO $ newMVar ()
                              return $ InitSync fter h lock
                       Just (InitSync fter' h lock) ->
                           liftIO $ do
                               return $ InitSync (liftM2 (||) fter' fter) h lock
                       Just (InitAsync fter' tchan h lock) ->
                           liftIO $ do
                               return $ Both fter h lock fter' tchan
                       Just (Both fter' h lock fter'' tchan) ->
                           liftIO $ do
                               return $ Both (liftM2 (||) fter fter') h lock fter'' tchan
               modify (Map.insert trueF nv)
        StdoutLogger -> register (hFlush stdout) >> return ()
        StderrLogger -> register (hFlush stderr) >> return ()
        _ -> return ()

asyncLogger :: Handle -> MVar () -> TChan String -> IO ()
asyncLogger h lock tchan = do
  forever $ do
    str <- atomically $ readTChan tchan
    withMVar lock $ \_ -> hPutStr h str

-- the first string is noncoloured, the second may be coloured depending on config
logger :: LoggerInternal -> LogLine -> String -> String -> IO ()
logger (AsyncFileLoggerInternal tchan) _ s _ = atomically $ writeTChan tchan s
logger (SyncFileLoggerInternal h lock) _ s _ = withMVar lock $ \_ -> hPutStr h s >> hFlush h
logger (StdoutLoggerInternal lock)     _ _ s = withMVar lock $ \_ -> hPutStr stdout s
logger (StderrLoggerInternal lock)     _ _ s = withMVar lock $ \_ -> hPutStr stderr s
logger (TChanLoggerInternal tchan)     l _ _ = atomically $ writeTChan tchan l

formatLine :: Bool -> LogFormat -> LogLine -> String
formatLine isColour les ll = concatMap (formatLine' ll) les ++ "\n"
    where
      formatLine' LogLine{logMessage} MessageElem    = logMessage
      formatLine' LogLine{logSeverity} SeverityElem  =
          (if isColour then withSgr (sgr logSeverity) else id) . padS 7 $ show logSeverity
      formatLine' _ (StringElem str)                 = str
      formatLine' LogLine{logTimestamp} (DateTimeElem f) = f logTimestamp
      formatLine' LogLine{logThread} ThreadElem      = logThread

instance (MonadBaseControl IO m, MonadIO m) => Forkable (SLogT m) (SLogT m) where
    fork (SLogT (ReaderT f)) = SLogT . ReaderT $ \env -> do
                                 fork $ do
                                   tid <- liftIO myThreadId
                                   f env { threadName = show tid }

forkSLog :: (MonadBaseControl IO m, MonadIO m) => String -> SLogT m () -> SLogT m ThreadId
forkSLog tname (SLogT m) = SLogT . local (\e -> e { threadName = tname }) $ fork m

padS :: Int -> String -> String
padS n = take n . (++ repeat ' ')

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