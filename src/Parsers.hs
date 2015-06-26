module Parsers where
import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.UTF8 as UTF8
import qualified Data.Text    as T
import           Data.Text.Encoding
import           System.IO
import           Text.Trifecta
import           Text.Trifecta.Delta

import           Types

lineLength :: Int
lineLength = 100

validLength :: String -> Parser ()
validLength section = do
  bl <- line
  let tl = T.length $ decodeUtf8 bl
  if tl > lineLength
    then raiseErr $ failed (section ++ " line too long. Maximum is " ++ show lineLength ++ " chars, got " ++ show tl ++ " chars.")
    else return ()

wrappedBy :: Char -> Char -> Parser String
wrappedBy l r = between (char l) (char r) $ many $ notChar r

scopeParser :: Parser T.Text
scopeParser = T.pack <$> wrappedBy '(' ')'

endOfLine :: Parser ()
endOfLine = optional (char '\r') *> char '\n' *> pure ()

comment = char '#' *> many notEOL *> endOfLine

comments = many comment *> pure ()

notEOL :: Parser Char
notEOL = noneOf "\r\n"

nextSection :: Parser ()
nextSection = endOfLine *> endOfLine *> pure ()

commitMessageParser :: Parser CommitMessage
commitMessageParser = do
  comments
  type_ <- typeParser <?> "Type"
  scope <- scopeParser <?> "Scope"
  string ": "
  subject <- T.pack <$> some notEOL <?> "Subject"
  validLength "Subject"
  comments
  endOfLine
  comments
  endOfLine
  comments
  body <- many $ do
    res <- T.pack <$> some notEOL <?> "Body"
    validLength "Body"
    endOfLine
    comments
    return res
  endOfLine
  comments
  footer <- many $ do
    res <- T.pack <$> some notEOL <?> "Footer"
    validLength "Footer"
    endOfLine
    comments
    return res
  comments
  many endOfLine
  comments
  return $ CommitMessage type_ scope subject body footer

gitLogOutputs :: Parser [CommitMessage]
gitLogOutputs = manyTill (commitMessageParser <* (string "==END==" *> endOfLine)) eof

parseFromHandle :: MonadIO m => Parser a -> Delta -> Handle -> m (Result a)
parseFromHandle p d h = go $ stepParser startP mempty mempty
  where
    startP = release d *> p
    go aStep = do
      bs <- liftIO $ B.hGet h 4096
      if B.null bs
        then return $ starve aStep
        else do
          let aStep' = feed bs aStep
          case aStep' of
            StepFail _ _ -> return $ starve aStep'
            _ -> go aStep'
  
match :: String -> a -> Parser a
match k v = string k *> pure v

typeParser :: Parser CommitType
typeParser = choice $ map (uncurry match)
  [ ("feature", Feature)
  , ("fix", Fix)
  , ("docs", Docs)
  , ("style", Style)
  , ("refactor", Refactor)
  , ("perf", Perf)
  , ("test", Test)
  , ("chore", Chore)
  ]

