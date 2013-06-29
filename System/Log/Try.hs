{-# LANGUAGE TemplateHaskell #-}
module Try where

import Format
import SLog
import Control.Concurrent
import Control.Monad.IO.Class

main :: IO ()
main = do
  m <- newMVar ()
  n <- newMVar ()
  runSLogT LogConfig { ansiColours = True
                     , loggers = [ (anySev, StderrLogger)
                                 -- , ((WARNING > ), StdoutLogger)
                                 , (anySev, SyncFileLogger "log2")
                                 ]
                     }
       (for $(mat "%d(%F %T) [%s] {%m}")) "main" $ do
         logW "asd"
         logE "das"
         () <- liftIO $ takeMVar m
         forkSLog "ASDASD" $ do
           liftIO $ takeMVar m
           liftIO $ takeMVar n
           logI "HEY" :: SLogT IO ()
           liftIO $ putMVar n ()
         logI "asd"
  putMVar m ()
  takeMVar n
