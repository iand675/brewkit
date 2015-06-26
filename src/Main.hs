{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Monad
import           Control.Monad.Trans
import           Text.PrettyPrint.ANSI.Leijen (Doc, putDoc, indent)
import           Text.Trifecta (Result(..), parseTest, parseByteString, eof)
import           Text.Trifecta.Delta
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Text       as T
import qualified Data.List       as L
import qualified Data.Map.Strict as S
import           Data.Monoid
import           Data.Text (Text)
import           Data.Text.Encoding
import qualified Data.Text.IO as T
import           Options.Applicative hiding (Success, Failure, Parser)
import           System.Console.ANSI
import           System.Directory
import           System.Exit
import           System.FilePath
import           Prelude             hiding (takeWhile)

import           Formatter
import           Git
import           Parsers
import           Types

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

main :: IO ()
main = do
  args <- execParser $ info commandParser idm
  case args of
    InstallHooks -> installCommitMessageHook
    ValidateCommitMessage path -> B.readFile path >>= validateCommitMessage path
    Changelog -> do
      commitResult <- parseFromHandle gitLogOutputs (Directed "git log" 0 0 0 0) =<< gitLogMessages
      case commitResult of
        Failure doc -> do
          putDoc doc
          exitFailure
        Success commits -> mapM_ print commits

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

{-
installMessageTemplateHook = do
  let templateHookPath = ".git/hooks/prepare-commit-msg"
  T.writeFile templateHookPath "#!/bin/sh/\n"
-}

type Scope = Text
type GroupedChangelog = S.Map CommitType (S.Map Scope CommitMessage)

