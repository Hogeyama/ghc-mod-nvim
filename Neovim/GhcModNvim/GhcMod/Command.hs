{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Neovim.GhcModNvim.GhcMod.Command where

import           Prelude                 hiding (log)

import           Neovim
import           Neovim.Context

import           Neovim.GhcModNvim.Utility
import           Neovim.GhcModNvim.Types

import           Data.Maybe              (isJust)
import           Control.Lens
import           Control.Monad           (forever, unless)
import           Control.Monad.Catch     (SomeException, catch)
import           System.IO
import           System.Process
import           System.Timeout          (timeout)

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

lint :: NeoGhcMod String
lint = neoGhcMod "lint" []

checkSyntax :: NeoGhcMod String
checkSyntax = neoGhcMod "check" []

info :: String -> NeoGhcMod String
info id' = neoGhcMod "info" [id']

types :: Int -> Int -> NeoGhcMod String
types line col = neoGhcMod "type" [show line, show col]

-------------------------------------------------------------------------------
-- Main Wrapper
-------------------------------------------------------------------------------

neoGhcMod :: String -> [String] -> NeoGhcMod String
neoGhcMod cmd args = do
  f <- nvimCurrentFile
  let cmd' = unwords $ cmd : f : args
  (hin,hout,_herr,_p) <- getHandles
  liftIO (hPutStrLn hin cmd')
  liftIO (readResult_ hout) >>= \case
    Right x -> return x
    Left e  -> err $ show e

-------------------------------------------------------------------------------
-- Handling ghc-modi
-------------------------------------------------------------------------------

getHandles :: NeoGhcMod (Handle,Handle,Handle,ProcessHandle)
getHandles = use ghcmodHandlers >>= \case
  Just hs@(_,_,_,p) -> liftIO (getProcessExitCode p) >>= \case
    Just _  -> createHandles
    Nothing -> return hs
  Nothing -> createHandles

createHandles :: NeoGhcMod (Handle,Handle,Handle,ProcessHandle)
createHandles = do
    root <- getRoot
    reportInfo "wakening ghc-modi.."
    hs <- createHandles' root
    ghcmodHandlers .= Just hs
    return hs
  where
    createHandles' :: FilePath -> NeoGhcMod (Handle,Handle,Handle,ProcessHandle)
    createHandles' root = do
        hs@(_hin,_hout,herr,p) <- liftIO $ wakeGhcMod root
        _thID <- forkNeovim () () (ghcmodErrorHandler herr)
          -- TODO _thIDの管理
        whenM (liftIO (isTerminated p)) $ reportErrorAndFail "ghc-mod terminated"
        return hs
      `catch`
        \(e::SomeException) -> err (show e)

    wakeGhcMod :: FilePath -> IO (Handle,Handle,Handle,ProcessHandle)
    wakeGhcMod root = do
        hs@(hin,hout,herr,_p) <- runInteractiveProcess
            "stack" ["exec","--","ghc-mod","legacy-interactive"] (Just root) Nothing
        hSetBuffering hin  LineBuffering
        hSetBuffering hout NoBuffering
        hSetBuffering herr NoBuffering
        return hs

    ghcmodErrorHandler :: Handle -> Neovim () () ()
    ghcmodErrorHandler herr = forever $ do
      s <- liftIO $ hGetLine herr
      unless (s == ignorableMessage) $ reportError s

    isTerminated :: ProcessHandle -> IO Bool
    isTerminated p = isJust <$> getProcessExitCode p

-------------------------------------------------------------------------------
-- Other Functions
-------------------------------------------------------------------------------

ignorableMessage :: String
ignorableMessage = "Warning: STACK_EXE set, preferring Stack project"

getRoot :: NeoGhcMod FilePath
getRoot = do
  cwd' <- nvimCwd
  let prc = (proc "ghc-mod" ["root"]) { cwd = Just cwd' }
  (_, ok, e) <- liftIO $ readCreateProcessWithExitCode prc ""
  if null e || init e == ignorableMessage
    then return $ init ok -- `init` removes a null character added by ghc-mod
    else err $ "ghc-mod root:\n" ++ e

readResult_ :: Handle -> IO (Either String String)
readResult_ h = bimap f f <$> go []
  where
    f     = unlines . reverse
    line  = timeout (int2sec 10) (hGetLine h)
    go ss = line >>= \case
      Nothing -> return (Left ["timeout"])
      Just s
        | take 2 s == "OK" -> return $ Right ss
        | take 2 s == "NG" -> return $ Left (s:ss)
        | otherwise        -> go (s:ss)

int2sec :: Int -> Int
int2sec = (*1000000)

whileJust :: Monad m => m (Maybe a) -> m [a]
whileJust m = reverse <$> go []
  where
    go acc = m >>= \case
      Nothing -> return acc
      Just x  -> go (x:acc)

