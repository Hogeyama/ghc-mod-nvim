{-# LANGUAGE RecordWildCards #-}

module Neovim.GhcModNvim.GhcMod.Parser (
    typeParser
  , infoParser
  , errorParser
  ) where

import           Text.Parsec              hiding (Error)
import           Text.Parsec.String       (Parser)
import qualified Text.Parsec.Token        as P
import           Text.Parsec.Language     (haskellDef)

import qualified Neovim.Quickfix          as Q
import           Neovim.Quickfix          (QuickfixErrorType(..))
import           Data.List                (sort)
import           Data.Functor             (void)
import           Data.Functor.Identity    (Identity)
import           System.FilePath.Posix    ((</>))
import           Data.Bifunctor           (bimap, first)
import           Data.Function            (on)
import           Neovim.GhcModNvim.Types  (TypeInfo(..))

-------------------------------------------------------------------------------
-- Parser for ghc-mod (check|lint)
-------------------------------------------------------------------------------

errorParser :: FilePath -> String -> Either String [Q.QuickfixListItem String]
errorParser basedir s =
    bimap show (concatMap toQFItems . sort) $ runParser manyErrorWarn () "" s
  where
    manyErrorWarn :: Parser [EWItem]
    manyErrorWarn = many $ ewParser basedir

    toQFItems :: EWItem -> [Q.QuickfixListItem String]
    toQFItems EWItem{..} = header : map strToQFItem messageEW
      where
        Pos{..} = postionEW
        header = createQFItem file line col errTypeEW

-- ^ One Error or Warning
data EWItem = EWItem {
    postionEW :: Pos
  , errTypeEW :: QuickfixErrorType
  , messageEW :: [String]
  } deriving (Eq)

instance Ord EWItem where
  compare = compare' `on` errTypeEW
    where compare' Error Warning = LT
          compare' Warning Error = GT
          compare' _ _ = EQ

ewParser :: FilePath -> Parser EWItem
ewParser basedir = do
  postionEW <- position basedir <* colon
  errTypeEW <- errType
  messageEW <- messages
  return EWItem{..}

-------------------------------------------------------------------------------
-- Parser for ghc-mod info
-------------------------------------------------------------------------------

infoParser :: FilePath -> String -> Either String [Q.QuickfixListItem String]
infoParser basedir s = mapLeft show $ runParser p () "" s
  where
    p = ((:) <$> lookAhead (try pos) <*> messagesQF) <|> messagesQF
    pos = do
      void $ manyTill anyToken (symbol' "Defined" >> symbol' "at")
      Pos{..}  <- position basedir
      return $ createQFItem file line col Warning

-------------------------------------------------------------------------------
-- Parser for ghc-mod type
-------------------------------------------------------------------------------

typeParser :: String -> Either String [TypeInfo]
typeParser s = mapLeft show $ runParser types () "" s

types :: Parser [TypeInfo]
types = many1 typeUnit
  where
    typeUnit = do
      (line1,col1) <- (,) <$> natural <*> natural
      (line2,col2) <- (,) <$> natural <*> natural
      ty <- stringLiteral
      return TypeInfo{..}

-------------------------------------------------------------------------------
-- Other Parsers
-------------------------------------------------------------------------------

-- '\0' seperated sentences
messagesQF :: Parser [Q.QuickfixListItem String]
messagesQF = map strToQFItem <$> messages

strToQFItem :: String -> Q.QuickfixListItem String
strToQFItem s = Q.QFItem { Q.bufOrFile     = Right ""
                         , Q.lnumOrPattern = Right ""
                         , Q.col           = Nothing
                         , Q.nr            = Nothing
                         , Q.text          = s
                         , Q.errorType     = Warning
                         }

-- only used in this file
data Pos = Pos {
    file :: FilePath
  , line :: Int
  , col  :: Int
  } deriving (Eq, Ord)

position :: FilePath -> Parser Pos
position basedir = do
  file <- manyTill anyChar colon
  line <- natural <* colon
  col  <- natural
  return $ Pos (basedir</>file) line col

errType :: Parser QuickfixErrorType
errType = do void $ symbol' "Warning"
             void colon
             return Warning
       <|> return Error

messages :: Parser [String]
messages = many (noneOf "\0\n") `sepBy` char '\0' <* (void endOfLine <|> eof)

-------------------------------------------------------------------------------
-- Lexer
-------------------------------------------------------------------------------

lexer :: P.GenTokenParser String u Data.Functor.Identity.Identity
lexer = P.makeTokenParser haskellDef

natural       :: Parser Int
symbol'       :: String -> Parser String
stringLiteral :: Parser String
colon         :: Parser String
natural       = fromInteger <$> P.natural lexer
symbol'       = try . P.symbol lexer
stringLiteral = P.stringLiteral lexer
colon         = P.colon lexer

-------------------------------------------------------------------------------
-- Other
-------------------------------------------------------------------------------

createQFItem :: FilePath -> Int -> Int -> QuickfixErrorType -> Q.QuickfixListItem String
createQFItem file line col ety = Q.QFItem {
    Q.bufOrFile     = Right file
  , Q.lnumOrPattern = Left line
  , Q.col           = Just (col, True)
  , Q.nr            = Nothing
  , Q.text          = ""
  , Q.errorType     = ety
  }

mapLeft :: (a -> b) -> Either a r -> Either b r
mapLeft = first

