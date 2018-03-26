{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Neovim.GhcModNvim.Plugin (
    neoGhcModCheck
  , neoGhcModInfo
  , neoGhcModLint
  , neoGhcModLintAll
  , neoGhcModType
  , neoGhcModTypeClear
  ) where

import           Neovim.GhcModNvim.Utility
import           Neovim.GhcModNvim.Types
import           Neovim.GhcModNvim.GhcMod.Parser
import qualified Neovim.GhcModNvim.GhcMod.Command as C

import           Neovim
import           Neovim.Quickfix                  (QuickfixAction (..),
                                                   QuickfixListItem, setqflist)

import           Control.Lens                     (use)
import           Control.Lens.Operators
import           Control.Monad                    (when)
import qualified Data.ByteString                  as B
import           Data.String                      (IsString (..))
import           System.FilePath.Posix            (makeRelative)
import           Control.Monad.Catch              (catch, SomeException)

-------------------------------------------------------------------------------
-- :NeoGhcModCheck
-------------------------------------------------------------------------------

neoGhcModCheck :: CommandArguments -> NeoGhcMod ()
neoGhcModCheck _ = catchAnyException $
  setQFListAndOpenUnlessEmpty =<< runParser errorParser =<< C.checkSyntax

-------------------------------------------------------------------------------
-- :NeoGhcModLint
-------------------------------------------------------------------------------

neoGhcModLint :: CommandArguments -> NeoGhcMod ()
neoGhcModLint _ = catchAnyException $
  setQFListAndOpenUnlessEmpty =<< runParser errorParser =<< C.lint

neoGhcModLintAll :: CommandArguments -> NeoGhcMod ()
neoGhcModLintAll _ = catchAnyException $
  setQFListAndOpenUnlessEmpty =<< runParser errorParser =<< C.lintAll

-------------------------------------------------------------------------------
-- :NeoGhcModInfo
-------------------------------------------------------------------------------

neoGhcModInfo :: CommandArguments -> Maybe String -> NeoGhcMod ()
neoGhcModInfo _ ms = catchAnyException $ do
  id' <- maybe nvimCword return ms
  qs  <- runParser infoParser =<< C.info id'
  setqflist qs Replace
  copen
  void $ vim_command "setlocal ft=haskell"

-------------------------------------------------------------------------------
-- :NeoGhcModType :NeoGhcModTypeClear
-------------------------------------------------------------------------------

neoGhcModType :: CommandArguments -> NeoGhcMod ()
neoGhcModType CommandArguments{ bang } = catchAnyException $ nvimCurrentPos >>= go
  where
    go :: NvimPos -> NeoGhcMod ()
    go pos@(NvimPos _ line col _) = clearTypeHilight >>
      use (typeState.tyList) >>= \case
        [] ->
            typeParser <$> C.types line col >>= \case
              Right []     -> error "NeoGhcModType: Impossible"
              Right (x:xs) -> display x >> typeState.tyList .= xs
              Left _e      -> reportError "Cannot guess type"
        (x@TypeInfo{..} : xs) ->
            if (line1,col1) <= (line,col) && (line,col) <= (line2,col2)
              then typeState.tyList .= xs >> display x
              else typeState.tyList .= [] >> go pos

    display :: TypeInfo -> NeoGhcMod ()
    display TypeInfo{..} = do
      let pat = B.concat [ "\\%", show' line1, "l", "\\%", show' col1 ,"c", "\\_.*"
                         , "\\%", show' line2, "l", "\\%", show' col2 ,"c" ]
      id' <- errOnInvalidResult $ vim_call_function
                "matchadd" [ObjectString "Search", ObjectString pat]
      typeState.matchID .= Just id'
      nvimOutWrite ty
      when (bang == Just True) $ setReg "\"" (fromString ty)

    setReg :: B.ByteString -> B.ByteString -> NeoGhcMod ()
    setReg reg str = void $ vim_call_function
      "setreg" [ObjectString reg, ObjectString str]

neoGhcModTypeClear :: CommandArguments -> NeoGhcMod ()
neoGhcModTypeClear _ = clearTypeHilight >> typeState.tyList .= []

clearTypeHilight :: NeoGhcMod ()
clearTypeHilight = use (typeState.matchID) >>= \case
  Just id' -> do
    void $ vim_call_function' "matchdelete" [ObjectString $ show' id']
    typeState.matchID .= Nothing
  Nothing -> return ()

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

runParser :: (FilePath -> String -> Either String [QuickfixListItem String])
          -> String -> NeoGhcMod [QuickfixListItem String]
runParser parser s = do
    basedir <- makeRelative <$> nvimCwd <*> C.getRoot
    case parser basedir s of
      Left  e  -> reportErrorAndFail $ unlines [s, "Parse Error:", e]
      Right qs -> return qs

setQFListAndOpenUnlessEmpty :: [QuickfixListItem String] -> NeoGhcMod ()
setQFListAndOpenUnlessEmpty qs = do
  setqflist qs Replace
  if null qs
    then cclose >> reportInfo "no errors found"
    else copen

catchAnyException :: NeoGhcMod () -> NeoGhcMod ()
catchAnyException m = m `catch` \case
  (e :: SomeException) -> reportError (show e)

show' :: (IsString s, Show a) => a -> s
show' = fromString . show

