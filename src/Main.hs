{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Monad
import           Control.Monad.Trans
import           Text.PrettyPrint.ANSI.Leijen (Doc, putDoc, indent)
import           Text.Trifecta
import           Text.Trifecta.Delta
import qualified Data.List    as L
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text    as T
import           Data.Text.Encoding
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.UTF8 as UTF8
import           Options.Applicative hiding (Success, Failure, Parser)
import           System.Console.ANSI
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process
import           Prelude             hiding (takeWhile)

data Command
  = InstallHooks
  | ValidateCommitMessage FilePath
  | Changelog
  deriving (Show)

validateCommitArgs = ValidateCommitMessage
  <$> (argument str (metavar "COMMIT_MSG_PATH"))

commandParser = subparser
  (  command "install" (info (pure InstallHooks) (progDesc "Install Git hooks for the brewtown workflow" ))
  <> command "validate:commit:message" (info validateCommitArgs (progDesc "Validate a commit message"))
  <> command "changelog" (info (pure Changelog) (progDesc "Generate changelog"))
  )

data CommitType = Feature
                | Fix
                | Docs
                | Style
                | Refactor
                | Perf
                | Test
                | Chore
                deriving (Show, Eq, Ord)

data CommitMessage = CommitMessage
  { commitMessageType    :: CommitType
  , commitMessageScope   :: Text
  , commitMessageSubject :: Text
  , commitMessageBody    :: [Text]
  , commitMessageFooter  :: [Text]
  } deriving (Show)

lineLength = 100

validLength section = do
  bl <- line
  let tl = T.length $ decodeUtf8 bl
  if tl > lineLength
    then raiseErr $ failed (section ++ " line too long. Maximum is " ++ show lineLength ++ " chars, got " ++ show tl ++ " chars.")
    else return ()

match k v = string k *> pure v

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

wrappedBy :: Char -> Char -> Parser String
wrappedBy l r = between (char l) (char r) $ many $ notChar r

scopeParser :: Parser Text
scopeParser = T.pack <$> wrappedBy '(' ')'

endOfLine = optional (char '\r') >> char '\n' >> return ()

notEOL :: Parser Char
notEOL = noneOf "\r\n"

nextSection :: Parser ()
nextSection = endOfLine *> endOfLine *> pure ()

commitMessageParser :: Parser CommitMessage
commitMessageParser = do
  type_ <- typeParser <?> "Type"
  scope <- scopeParser <?> "Scope"
  string ": "
  subject <- T.pack <$> some notEOL <?> "Subject"
  validLength "Subject"
  endOfLine
  endOfLine
  body <- many $ do
    res <- T.pack <$> some notEOL <?> "Body"
    validLength "Body"
    endOfLine
    return res
  endOfLine
  footer <- many $ do
    res <- T.pack <$> some notEOL <?> "Footer"
    validLength "Footer"
    endOfLine
    return res
  many endOfLine
  return $ CommitMessage type_ scope subject body footer

main :: IO ()
main = do
  args <- execParser $ info commandParser idm
  case args of
    InstallHooks -> installCommitMessageHook
    ValidateCommitMessage path -> B.readFile path >>= validateCommitMessage path
    Changelog -> return ()

parseCommitMessage :: Text -> IO ()
parseCommitMessage = parseTest commitMessageParser . T.unpack

validateCommitMessage :: FilePath -> B.ByteString -> IO ()
validateCommitMessage path str = do
  T.putStr "Checking commit message format "
  let draftPath = replaceBaseName path (takeBaseName path ++ "_DRAFT")
  case parseByteString (commitMessageParser <* eof) (Directed (UTF8.fromString path) 0 0 0 0) str of
    Failure doc -> do
      setSGR [SetColor Foreground Dull Red]
      T.putStrLn "✗"
      setSGR [Reset]
      putDoc $ indent 2 doc
      T.putStr "\n"
      copyFile path draftPath
      exitFailure
    _ -> do
      setSGR [SetColor Foreground Dull Green]
      T.putStrLn "✓"
      setSGR [Reset]

test :: Text
test = "fix(Network.Mandrill): use correct lens names\n\nIt was broken, now it isn't.\nNot too tough to fix, really.\n\nFixes #1234\n"

installCommitMessageHook :: IO ()
installCommitMessageHook = do
  let commitMsgHookPath = ".git/hooks/commit-msg"
  T.writeFile commitMsgHookPath "#!/bin/sh\nbrewkit validate:commit:message $1\n"
  perms <- getPermissions commitMsgHookPath
  setPermissions commitMsgHookPath $ perms { executable = True }

gitLogOutputs :: Parser [CommitMessage]
gitLogOutputs = sepBy1 commitMessageParser $ string "----END_OF_COMMIT----"

gitLogMessages :: IO Handle
gitLogMessages = do
  (_, Just out, _, _) <- createProcess $ p { std_out = CreatePipe }
  return out
  where
    p = proc "git" ["log", "--format=\"%B%n----END_OF_COMMIT----\""]

parseFromHandle :: MonadIO m => Parser a -> Delta -> Handle -> m (Result a)
parseFromHandle p d h = go $ stepParser startP mempty mempty
  where
    startP = release d *> p
    go aStep = do
      bs <- liftIO $ B.hGet h 4096
      if bs == B.empty
        then return $ starve aStep
        else do
          let aStep' = feed bs aStep
          case aStep' of
            StepFail _ _ -> return $ starve aStep'
            _ -> go aStep'
  
{-
installMessageTemplateHook = do
  let templateHookPath = ".git/hooks/prepare-commit-msg"
  T.writeFile templateHookPath "#!/bin/sh/\n"
-}

