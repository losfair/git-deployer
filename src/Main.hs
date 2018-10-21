module Main where

import qualified GitDeployer.Config
import qualified GitDeployer.Local
import qualified GitDeployer.Query
import qualified System.Environment as Env

main :: IO ()
main = do
    (cfgPath:_) <- Env.getArgs
    cfg <- GitDeployer.Config.loadFromYamlFile cfgPath
    GitDeployer.Query.periodicallyQueryAll cfg
