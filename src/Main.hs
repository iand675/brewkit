{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Monad
import           Data.Attoparsec.Text
import qualified Data.List    as L
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           Options.Applicative hiding (Success, Failure)
import           System.Directory
import           System.Exit
import           Prelude             hiding (takeWhile)

data Command
  = InstallHooks
  | ValidateCommitMessage FilePath
  deriving (Read, Show)

validateCommitArgs = ValidateCommitMessage
  <$> (argument str (metavar "COMMIT_MSG_PATH"))

commandParser = subparser
  (  command "install" (info (pure InstallHooks) (progDesc "Install Git hooks for the brewtown workflow" ))
  <> command "validate:commit:message" (info validateCommitArgs (progDesc "Validate a commit message"))
  )

data CommitType = Feature
                | Fix
                | Docs
                | Style
                | Refactor
                | Perf
                | Test
                | Chore
                deriving (Show, Eq)

data CommitMessage = CommitMessage
  { commitMessageType    :: CommitType
  , commitMessageScope   :: Text
  , commitMessageSubject :: Text
  , commitMessageBody    :: [Text]
  , commitMessageFooter  :: [Text]
  } deriving (Show)

data ValidationFailure = ParseFailure String
                       | HeaderTooLong
                       | LineTooLong Text
                       deriving (Show)

lineLength = 100

validateCommitMessage raw = case parseCommitMessage raw of
  Left err -> Left [ParseFailure err]
  Right msg -> let errs = (validateHeader msg <> validateBody msg <> validateFooter msg) in case errs of
    [] -> Right msg
    es -> Left es

validateHeader m = if (4 + length (show $ commitMessageType m) + T.length (commitMessageScope m) + T.length (commitMessageSubject m)) <= lineLength
  then []
  else [HeaderTooLong]

validateFooter m = concatMap (\l -> if T.length l <= lineLength then [] else [LineTooLong l]) (commitMessageFooter m)

validateBody   m = concatMap (\l -> if T.length l <= lineLength then [] else [LineTooLong l]) (commitMessageBody m)

typeParser = choice [ string "feature"  *> pure Feature
                    , string "fix"      *> pure Fix
                    , string "docs"     *> pure Docs
                    , string "style"    *> pure Style
                    , string "refactor" *> pure Refactor
                    , string "perf"     *> pure Perf
                    , string "test"     *> pure Test
                    , string "chore"    *> pure Chore
                    ]

scopeParser = do
  char '('
  s <- takeWhile (/= ')')
  char ')'
  return s

commitMessageParser = do
  type_ <- typeParser <?> "Type"
  scope <- scopeParser <?> "Scope"
  string ": "
  subject <- takeWhile1 (not . isEndOfLine) <?> "Subject"
  endOfLine
  endOfLine
  body <- manyTill (takeWhile1 (not . isEndOfLine) <* endOfLine) endOfLine <?> "Body"
  footer <- manyTill (takeWhile1 (not . isEndOfLine) <* endOfLine) (endOfLine <|> endOfInput) <?> "Footer"
  return $ CommitMessage type_ scope subject body footer

main = do
  args <- execParser $ info commandParser idm
  case args of
    InstallHooks -> do
      let commitMsgHookPath = ".git/hooks/commit-msg"
      T.writeFile commitMsgHookPath "#!/bin/sh\nbrewkit validate:commit:message $1\n"
      perms <- getPermissions commitMsgHookPath
      setPermissions commitMsgHookPath $ perms { executable = True }
    ValidateCommitMessage path -> do
      T.putStrLn "Validating commit message format..."
      msg <- T.readFile path
      case validateCommitMessage msg of
        Left errs -> do
          forM_ errs $ \err -> T.putStrLn $ case err of
            ParseFailure str -> "Message parse failure: " <> T.pack str
            HeaderTooLong    -> "Header is too long"
            LineTooLong l    -> "Line too long: " <> l
          exitFailure
        Right _ -> return ()

parseCommitMessage = parseOnly commitMessageParser

test :: Text
test = "fix(Network.Mandrill): use correct lens names\n\nIt was broken, now it isn't.\nNot too tough to fix, really.\n\nFixes #1234\n"


