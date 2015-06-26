module Git where
import           System.IO
import           System.Process
import           System.Directory

gitLogMessages :: IO Handle
gitLogMessages = do
  (_, Just out, _, _) <- createProcess $ p { std_out = CreatePipe }
  return out
  where
    p = proc "git" ["log", "--format=%B%n==END=="]

