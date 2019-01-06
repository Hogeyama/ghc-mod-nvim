{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Neovim.GhcModNvim.GhcMod.Command where

import           Prelude

import           Neovim
import           Neovim.Context
import           Neovim.Context.Internal

import           Neovim.GhcModNvim.Utility
import           Neovim.GhcModNvim.Types

import           Data.Maybe              (isJust)
import           Control.Lens
import           Control.Monad           (forever, unless)
import           Data.String             (IsString(..))
import           System.IO
import           System.Process
import           System.Timeout          (timeout)
import           System.Exit             (exitSuccess)
import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Monad.Extra     (ifM)
import           Control.DeepSeq (NFData)
import           UnliftIO.Exception

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

checkSyntax :: NeoGhcMod String
checkSyntax = neoGhcMod "check" . (:[]) =<< nvimCurrentFile

lint :: NeoGhcMod String
lint = neoGhcMod "lint" . (:[]) =<< nvimCurrentFile

lintAll :: NeoGhcMod String
lintAll = do
    s <- neoGhcMod "lint" ["."]
    if s == oldVersionErrorMsg
      then err (fromString oldVersionError)
      else return s
  where
    oldVersionErrorMsg
      = "Dummy:0:0:Error:.: openFile: inappropriate type (is a directory)\n"
    oldVersionError
      = "This version of ghc-mod does not accept lint commadn for directory"
    -- TODO display the version

info :: String -> NeoGhcMod String
info id' = neoGhcMod "info" . (:[id']) =<< nvimCurrentFile

types :: Int -> Int -> NeoGhcMod String
types line col = neoGhcMod "type" . (:[show line, show col]) =<< nvimCurrentFile

-------------------------------------------------------------------------------
-- Main Wrapper
-------------------------------------------------------------------------------

neoGhcMod :: String -> [String] -> NeoGhcMod String
neoGhcMod cmd args = do
  let cmd' = unwords $ cmd : args
  (hin,hout,_herr,_p) <- obtainHandles
  liftIO (hPutStrLn hin cmd')
  liftIO (readResult_ hout) >>= \case
    Right x -> return x
    Left e  -> err $ fromString $ show e

-------------------------------------------------------------------------------
-- Handling ghc-modi
-------------------------------------------------------------------------------

obtainHandles :: NeoGhcMod (Handle,Handle,Handle,ProcessHandle)
obtainHandles = useTV handles >>= \case
  Just hs@(_,_,_,p) -> liftIO (getProcessExitCode p) >>= \case
    Just _  -> createHandles
    Nothing -> return hs
  Nothing -> createHandles

createHandles :: NeoGhcMod (Handle,Handle,Handle,ProcessHandle)
createHandles = do
    root <- getRoot
    reportInfo "wakening ghc-modi.."
    hs <- createHandles' root
    assignTV handles (Just hs)
    return hs
  where
    createHandles' :: FilePath -> NeoGhcMod (Handle,Handle,Handle,ProcessHandle)
    createHandles' root = handleAny (err.fromString.show) $ do
        hs@(_hin,_hout,herr,p) <- liftIO $ wakeGhcMod root
        _thID <- forkNeovim () (ghcmodErrorHandler herr)
          -- TODO _thIDの管理
        whenM (liftIO (isTerminated p)) $ reportErrorAndFail "ghc-mod terminated"
        return hs

    wakeGhcMod :: FilePath -> IO (Handle,Handle,Handle,ProcessHandle)
    wakeGhcMod root = do
        hs@(hin,hout,herr,_p) <- runInteractiveProcess
            "stack" ["exec","--","ghc-mod","legacy-interactive"] (Just root) Nothing
        hSetBuffering hin  LineBuffering
        hSetBuffering hout NoBuffering
        hSetBuffering herr NoBuffering
        return hs

    ghcmodErrorHandler :: Handle -> Neovim () ()
    ghcmodErrorHandler herr = forever $ do
      b <- liftIO (hIsClosed herr)
      if b
        then liftIO exitSuccess
        else do
          s <- liftIO $ hGetLine herr
          unless (s == ignorableMessage) $ reportError ("グワーッ: "++s)

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
  let prc = (proc "stack" ["exec", "--", "ghc-mod", "root"]) { cwd = Just cwd' }
  (_, ok, e) <- liftIO $ readCreateProcessWithExitCode prc ""
  if null e || init e == ignorableMessage
    then return $ init ok -- `init` removes a null character added by ghc-mod
    else err $ fromString $ "ghc-mod root:\n" ++ e

readResult_ :: Handle -> IO (Either String String)
readResult_ h = bimap f f <$> go []
  where
    f     = unlines . reverse
    line  = timeout (int2sec 10) $
              ifM (hIsEOF h)
                (do threadDelay (int2sec 100)
                    error "impossible")
                (hGetLine h)
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


forkNeovim :: forall env env' a. NFData a => env' -> Neovim env' a -> Neovim env ()
forkNeovim r a = do
  cfg <- ask'
  let threadConfig = cfg
        { pluginSettings = Nothing -- <- slightly problematic
        , customConfig = r
        }
  _ <- liftIO . forkIO . void $ runNeovim threadConfig a
  return ()


