module GitDeployer.Query where

import qualified Control.Exception as Exception
import qualified Control.Concurrent as Concurrent
import qualified GitDeployer.Config as Config
import qualified GitDeployer.Local as Local
import qualified Network.Wreq as Wreq
import qualified Data.ByteString.Builder as BSBuilder
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HMStrict
import qualified Control.Concurrent.MVar as MVar
import Control.Lens

queryOne :: Config.RepoConfig -> IO ()
queryOne cfg = case Config.provider cfg of
    "GitHub" -> do
        res <- Wreq.getWith
                (Wreq.defaults & Wreq.auth ?~ Wreq.oauth2Token (BSLazy.toStrict . BSBuilder.toLazyByteString $ BSBuilder.stringUtf8 $ Config.providerToken cfg)) $
                "https://api.github.com/repos/" ++ Config.remoteRepo cfg ++ "/commits/" ++ Config.branch cfg
        let (Just (Aeson.Object map)) = Aeson.decode (res ^. Wreq.responseBody)
        let (Just (Aeson.String sha)) = HMStrict.lookup (Text.pack "sha") map

        Local.compareAndUpdate (Local.Repo { Local.repoConfig = cfg}) (Text.unpack sha)
        putStrLn $ "done querying GitHub repo " ++ Config.remoteRepo cfg

queryAll :: Config.Config -> IO ()
queryAll cfg = do
    mvs <- queryAllRepos $ Config.repos cfg
    waitForAll mvs
    where
        waitForAll :: [MVar.MVar ()] -> IO ()
        waitForAll [] = return ()
        waitForAll (x:xs) = do
            MVar.takeMVar x
            waitForAll xs

        queryAllRepos :: [Config.RepoConfig] -> IO [MVar.MVar ()]
        queryAllRepos [] = return []
        queryAllRepos (x:xs) = do
            mv <- MVar.newEmptyMVar :: IO (MVar.MVar ())
            Concurrent.forkFinally
                (Exception.handle (\(Exception.SomeException e) -> putStrLn $ "unable to query " ++ Config.provider x ++ " repo " ++ Config.remoteRepo x ++ ": " ++ show e) $ queryOne x)
                (\_ -> MVar.putMVar mv ())
            fmap ([mv] ++) (queryAllRepos xs)

periodicallyQueryAll :: Config.Config -> IO ()
periodicallyQueryAll cfg = do
    putStrLn "start queryAll"
    queryAll cfg
    Concurrent.threadDelay (5000000) -- micros
    periodicallyQueryAll cfg
