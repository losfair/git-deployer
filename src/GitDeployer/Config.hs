{-# LANGUAGE DeriveGeneric #-}

module GitDeployer.Config where

import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import GHC.Generics

data Config = Config { repos :: [RepoConfig] } deriving (Show, Generic)
data RepoConfig = RepoConfig {
    provider :: String,
    providerToken :: String,
    remoteRepo :: String,
    branch :: String,
    localPath :: String
} deriving (Show, Generic)

instance Aeson.ToJSON Config
instance Aeson.FromJSON Config

instance Aeson.ToJSON RepoConfig
instance Aeson.FromJSON RepoConfig

loadFromJsonFile :: String -> IO Config
loadFromJsonFile path = do
    out <- Aeson.decodeFileStrict path
    case out of
        Just v -> return v
        Nothing -> error "cannot load config from file"

loadFromYamlFile :: String -> IO Config
loadFromYamlFile path = Yaml.decodeFileThrow path

remoteUrl :: RepoConfig -> String
remoteUrl repo = case provider repo of
    "GitHub" -> "https://github.com/" ++ remoteRepo repo
