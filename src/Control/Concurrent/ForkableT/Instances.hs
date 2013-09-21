{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module Control.Concurrent.ForkableT.Instances where

import Control.Concurrent.ForkableT

import Control.Monad.IO.Class
import qualified Control.Monad.Trans.Resource as RS

instance (RS.MonadBaseControl IO m, MonadIO m) => Forkable (RS.ResourceT m) (RS.ResourceT m) where
    fork = RS.resourceForkIO
