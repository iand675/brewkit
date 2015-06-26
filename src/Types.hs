module Types where
import Data.Text (Text)

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
