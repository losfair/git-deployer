module GitDeployer.Local where

import qualified System.Process as Process
import qualified GitDeployer.Config as Config
import GHC.IO.Handle
import qualified Data.Text as Text
import System.Exit (ExitCode(..))

data Repo = Repo { repoConfig :: Config.RepoConfig }
type CommitId = String

execCommand :: Repo -> String -> [String] -> IO (ExitCode, String)
execCommand repo cmd args = do
    (_, Just hout, _, ph) <- Process.createProcess (Process.proc cmd args) {
        Process.cwd = Just $ (Config.localPath . repoConfig) repo,
        Process.std_out = Process.CreatePipe
    }
    ec <- Process.waitForProcess ph
    contents <- hGetContents hout
    return (ec, contents)

mustExecCommand :: String -> Repo -> String -> [String] -> IO String
mustExecCommand msg repo cmd args = do
    (ec, contents) <- execCommand repo cmd args
    case ec of
        ExitSuccess -> return contents
        ExitFailure code -> fail $ msg ++ " error code = " ++ show code

execGitCommand :: Repo -> [String] -> IO (ExitCode, String)
execGitCommand repo args = execCommand repo "git" args

mustExecGitCommand :: String -> Repo -> [String] -> IO String
mustExecGitCommand msg repo args = mustExecCommand msg repo "git" args

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

    case Config.deployCommand $ repoConfig repo of
        Just cmd -> do
            out <- mustExecCommand "cannot execute deploy command." repo "sh" ["-c", cmd]
            putStrLn $ "deploy stdout: " ++ out
        Nothing -> return ()

    return ()
