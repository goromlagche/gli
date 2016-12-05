module Main where

import           Data.Maybe
import           Data.Yaml
import           Gli.Gitlab
import           Gli.Types

main :: IO ()
main = do
  cfg <- decodeFile "/Users/goromlagche/.gli.yml" :: IO (Maybe GliCfg)
  allAccounts(fromJust cfg)
