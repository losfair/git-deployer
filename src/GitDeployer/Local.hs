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

mustExecGitCommand :: String -> Repo -> [String] -> IO String
mustExecGitCommand msg repo args = do
    (ec, contents) <- execGitCommand repo args
    case ec of
        ExitSuccess -> return contents
        ExitFailure code -> error $ msg ++ " error code = " ++ show code

lastCommitId :: Repo -> IO CommitId
lastCommitId repo = fmap (Text.unpack . Text.strip . Text.pack . snd) $ execGitCommand repo ["rev-parse", Config.branch $ repoConfig repo]

compareAndUpdate :: Repo -> CommitId -> IO ()
compareAndUpdate repo remoteCommit = do
    localCommit <- lastCommitId repo
    case take 16 localCommit == take 16 remoteCommit of
        True -> return ()
        False -> fetchAndUpdate repo

fetchAndUpdate :: Repo -> IO ()
fetchAndUpdate repo = do
    let branch = Config.branch $ repoConfig repo
    mustExecGitCommand "cannot checkout local branch." repo ["checkout", branch]
    mustExecGitCommand "cannot hard-reset local branch." repo ["reset", "--hard"]

    execGitCommand repo ["remote", "remove", "gd-remote"] -- this can fail if gd-remote didn't exist before.

    mustExecGitCommand "cannot add gd-remote." repo ["remote", "add", "gd-remote", (Config.remoteUrl . repoConfig) repo]
    mustExecGitCommand "cannot fetch from gd-remote." repo ["fetch", "gd-remote", branch]
    mustExecGitCommand "cannot merge remote branch into local branch (fast-forward)." repo ["merge", "--ff-only", "gd-remote/" ++ branch]

    return ()
