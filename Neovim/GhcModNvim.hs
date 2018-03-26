{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Neovim.GhcModNvim (plugin) where

import Neovim
import Neovim.GhcModNvim.Types
import Neovim.GhcModNvim.Plugin

plugin :: Neovim (StartupConfig NeovimConfig) () NeovimPlugin
plugin = wrapPlugin Plugin
  { exports = []
  , statefulExports =
    [ StatefulFunctionality
        { readOnly = ()
        , writable = initialState
        , functionalities =
          [ $(command' 'neoGhcModType)      ["async", "!"]
          , $(command' 'neoGhcModTypeClear) ["async"]
          , $(command' 'neoGhcModCheck)     ["async"]
          , $(command' 'neoGhcModLint)      ["async"]
          , $(command' 'neoGhcModLintAll)   ["async"]
          , $(command' 'neoGhcModInfo)      ["async"]
          ]
      }
    ]
  }

