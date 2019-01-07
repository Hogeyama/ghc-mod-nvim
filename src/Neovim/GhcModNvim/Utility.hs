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
  , show'
  , nvimCword
  , nvimCWORD
  , nvimOutWriteLn
  , reportInfo
  , reportError
  , reportErrorAndFail
  , reportAnyException
  , fromObject_
  ) where

import           Neovim
import           UnliftIO
import           Data.String (IsString(..))
import           Control.Lens

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

nvimCwd :: Neovim env String
nvimCwd = errOnInvalidResult $ vim_call_function "getcwd" []

nvimCurrentFile :: Neovim env String
nvimCurrentFile = errOnInvalidResult $ vim_call_function "expand" [ObjectString "%:p"]

nvimCword :: Neovim env String
nvimCword = errOnInvalidResult $ vim_call_function "expand" [ObjectString "<cword>"]

nvimCWORD :: Neovim env String
nvimCWORD = errOnInvalidResult $ vim_call_function "expand" [ObjectString "<cWORD>"]

nvimCurrentPos :: Neovim env NvimPos
nvimCurrentPos = do
  [bufnum, lnum, col, off] <- errOnInvalidResult $ vim_call_function "getpos" [ObjectString "."]
  return $ NvimPos bufnum lnum col off

copen :: Neovim env ()
copen = void $ vim_command "copen"

cclose :: Neovim env ()
cclose = void $ vim_command "cclose"

-------------------------------------------------------------------------------
-- Echo, Error
-------------------------------------------------------------------------------

nvimOutWriteLn :: String -> Neovim env ()
nvimOutWriteLn s = void $ nvim_out_write $ s ++ "\n"

pluginName :: String
pluginName = "ghc-mod-nvim"

reportInfo :: String -> Neovim env ()
reportInfo s = nvimOutWriteLn s'
  where s' = pluginName ++ ": " ++ s

reportError :: String -> Neovim env ()
reportError s = vim_report_error' s'
  where s' = pluginName ++ ": " ++ s

reportErrorAndFail :: String -> Neovim env a
reportErrorAndFail s = reportError s >> err (fromString s)

reportAnyException :: Neovim env () -> Neovim env ()
reportAnyException m = m `catchAny` (reportError . show)

fromObject_ :: NvimObject o => Object -> Either NeovimException o
fromObject_ o = case fromObject o of
  Right x -> Right x
  Left  e -> Left (ErrorMessage e)

show' :: (IsString s, Show a) => a -> s
show' = fromString . show

