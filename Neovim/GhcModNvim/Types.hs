{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

module Neovim.GhcModNvim.Types where

import Neovim                 (Neovim)
import System.IO              (Handle)
import System.IO.Unsafe       (unsafePerformIO)
import System.Process         (ProcessHandle)
import Control.Lens
import Control.Concurrent.STM
import Control.Monad.Reader.Class
import Control.Monad.IO.Class

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type NeoGhcMod = Neovim Env

data Env = Env
  { envHandles   :: !(TVar (Maybe (Handle,Handle,Handle,ProcessHandle)))
  , envTypeState :: !TypeState
  }

data TypeState = TypeState
  { _tyList  :: !(TVar [TypeInfo])
  , _matchID :: !(TVar (Maybe Int))
  }

data TypeInfo = TypeInfo
  { line1 :: Int
  , line2 :: Int
  , col1  :: Int
  , col2  :: Int
  , ty    :: String
  }

initialEnv :: Env
initialEnv = Env {
    envHandles   = unsafePerformIO $ newTVarIO Nothing
  , envTypeState = TypeState (unsafePerformIO $ newTVarIO [])
                             (unsafePerformIO $ newTVarIO Nothing)
  }
{-# NOINLINE initialEnv #-}

makeLenses ''TypeState
makeLensesWith camelCaseFields ''Env

useTV :: (MonadReader r m, MonadIO m) => Lens' r (TVar a) -> m a
useTV getter = liftIO . readTVarIO =<< view getter

usesTV :: (MonadReader r m, MonadIO m) => Lens' r (TVar a) -> (a -> b) -> m b
usesTV getter f = f <$> useTV getter

assignTV :: (MonadReader r m, MonadIO m) => Lens' r (TVar a) -> a -> m ()
assignTV getter x = do
  v <- view getter
  liftIO $ atomically $ writeTVar v x

