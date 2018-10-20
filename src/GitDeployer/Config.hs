{-# LANGUAGE DeriveGeneric #-}

module GitDeployer.Config where

import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import GHC.Generics

data Config = Config { repos :: RepoConfig } deriving (Show, Generic)
data RepoConfig = RepoConfig {
    provider :: Provider,
    remotePath :: String,
    branch :: String,
    localPath :: String
} deriving (Show, Generic)
data Provider = GitHub | GitLab deriving (Show, Generic)

instance Aeson.ToJSON Config
instance Aeson.FromJSON Config

instance Aeson.ToJSON RepoConfig
instance Aeson.FromJSON RepoConfig

instance Aeson.ToJSON Provider
instance Aeson.FromJSON Provider

loadFromJsonFile :: String -> IO Config
loadFromJsonFile path = do
    out <- Aeson.decodeFileStrict path
    case out of
        Just v -> return v
        Nothing -> error "cannot load config from file"

loadFromYamlFile :: String -> IO Config
loadFromYamlFile path = Yaml.decodeFileThrow path
