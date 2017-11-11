{-# LANGUAGE TemplateHaskell #-}

module Neovim.GhcModNvim.Types where

import Neovim                 (Neovim)
import System.IO              (Handle)
import System.Process         (ProcessHandle)
import Control.Lens           (makeLenses)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type NeoGhcMod = Neovim () NeoGhcModState

data NeoGhcModState = NeoGhcModState {
    _ghcmodHandlers :: Maybe (Handle,Handle,Handle,ProcessHandle)
  , _typeState      :: GhcModTypeState
  }

data GhcModTypeState = GhcModTypeState {
    _tyList  :: [TypeInfo]
  , _matchID :: Maybe Int
  }

data TypeInfo = TypeInfo {
    line1 :: Int
  , line2 :: Int
  , col1  :: Int
  , col2  :: Int
  , ty   :: String
  }

initialState :: NeoGhcModState
initialState = NeoGhcModState {
    _ghcmodHandlers = Nothing
  , _typeState      = GhcModTypeState [] Nothing
  }

makeLenses ''GhcModTypeState
makeLenses ''NeoGhcModState

