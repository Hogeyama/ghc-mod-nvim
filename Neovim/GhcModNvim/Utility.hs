{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Neovim.GhcModNvim.Utility (
    nvimCurrentFile
  , nvimCwd
  , nvimCurrentPos
  , NvimPos(..)
  , nvBufnum, nvCol, nvLnum, nvOff
  , copen
  , cclose
  , nvimCword
  , nvimCWORD
  , nvimOutWrite
  , reportInfo
  , reportError
  , reportErrorAndFail
  , withReportAllException
  , fromObject_
  ) where

import           Neovim
import           Prelude
import           Control.Lens
import           Control.Monad.Catch    (catch, SomeException)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data NvimPos = NvimPos
  { _nvBufnum :: Int
  , _nvLnum   :: Int
  , _nvCol    :: Int
  , _nvOff    :: Int
  } deriving (Show, Eq)
makeLenses ''NvimPos

-------------------------------------------------------------------------------
-- Useful commands
-------------------------------------------------------------------------------

nvimCwd :: Neovim r st String
nvimCwd = errOnInvalidResult $ vim_call_function "getcwd" []

nvimCurrentFile :: Neovim r st String
nvimCurrentFile = errOnInvalidResult $ vim_call_function "expand" [ObjectString "%:p"]

nvimCword :: Neovim r st String
nvimCword = errOnInvalidResult $ vim_call_function "expand" [ObjectString "<cword>"]

nvimCWORD :: Neovim r st String
nvimCWORD = errOnInvalidResult $ vim_call_function "expand" [ObjectString "<cWORD>"]

nvimCurrentPos :: Neovim r st NvimPos
nvimCurrentPos = do
  [bufnum, lnum, col, off] <- errOnInvalidResult $ vim_call_function "getpos" [ObjectString "."]
  return $ NvimPos bufnum lnum col off

-- TODO let copen command be customizable
copen :: Neovim r st ()
copen = void $ vim_command "botright copen"

cclose :: Neovim r st ()
cclose = void $ vim_command "cclose"

-------------------------------------------------------------------------------
-- Echo, Error
-------------------------------------------------------------------------------

nvimOutWrite :: String -> Neovim r st ()
nvimOutWrite s = void $ nvim_out_write $ s ++ "\n"

-- do not call 'reportErrorAndFail' in argument (or it will be reported twice)
withReportAllException :: Neovim r st a -> Neovim r st a
withReportAllException = flip catch $ \(e::SomeException) -> reportErrorAndFail (show e)

pluginName :: String
pluginName = "ghc-mod-nvim"

reportInfo :: String -> Neovim r st ()
reportInfo s = nvimOutWrite s'
  where s' = pluginName ++ ": " ++ s

reportError :: String -> Neovim r st ()
reportError s = vim_report_error' s'
  where s' = pluginName ++ ": " ++ s

reportErrorAndFail :: String -> Neovim r st a
reportErrorAndFail s = reportError s >> err s

fromObject_ :: NvimObject o => Object -> Either NeovimException o
fromObject_ o = case fromObject o of
  Right x -> Right x
  Left  e -> Left (ErrorMessage e)

