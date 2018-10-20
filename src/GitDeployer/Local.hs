module GitDeployer.Local where

import qualified System.Process as Process
import qualified GitDeployer.Config as Config
import GHC.IO.Handle
import qualified Data.Text as Text
import System.Exit (ExitCode(..))

data Repo = Repo { repoConfig :: Config.RepoConfig }
type CommitId = String

execGitCommand :: Repo -> [String] -> IO (ExitCode, String)
execGitCommand repo args = do
    (_, Just hout, _, ph) <- Process.createProcess (Process.proc "git" args) {
        Process.cwd = Just $ (Config.localPath . repoConfig) repo,
        Process.std_out = Process.CreatePipe
    }
    ec <- Process.waitForProcess ph
    contents <- hGetContents hout
    return (ec, contents)

unwrapGitResultWithMsg :: String -> (ExitCode, String) -> String
unwrapGitResultWithMsg msg (ec, contents) = case ec of
    ExitSuccess -> contents
    ExitFailure code -> error $ msg ++ " error code = " ++ show code

unwrapGitResult :: (ExitCode, String) -> String
unwrapGitResult = unwrapGitResultWithMsg "git exited with error."

lastCommitId :: Repo -> String -> IO CommitId
lastCommitId repo branch = fmap (Text.unpack . Text.strip . Text.pack . snd) $ execGitCommand repo ["rev-parse", branch]

compareAndUpdate :: Repo -> String -> CommitId -> IO ()
compareAndUpdate repo branch remoteCommit = do
    localCommit <- lastCommitId repo branch
    case take 16 localCommit == take 16 remoteCommit of
        True -> return ()
        False -> fetchAndUpdate repo branch

fetchAndUpdate :: Repo -> String -> IO ()
fetchAndUpdate repo branch = do
    fmap (unwrapGitResultWithMsg "cannot checkout local branch.") $ execGitCommand repo ["checkout", branch]
    fmap (unwrapGitResultWithMsg "cannot hard-reset local branch.") $ execGitCommand repo ["reset", "--hard"]

    execGitCommand repo ["remote", "remove", "gd-remote"] -- this can fail if gd-remote didn't exist before.

    fmap (unwrapGitResultWithMsg "cannot add gd-remote.") $ execGitCommand repo ["remote", "add", "gd-remote", (Config.remotePath . repoConfig) repo]
    fmap (unwrapGitResultWithMsg "cannot fetch from gd-remote.") $ execGitCommand repo ["fetch", "gd-remote", branch]
    fmap (unwrapGitResultWithMsg "cannot merge remote branch into local branch (fast-forward).") $ execGitCommand repo ["merge", "--ff-only", "gd-remote/" ++ branch]

    return ()
