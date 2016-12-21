{-# LANGUAGE OverloadedStrings #-}

module Gli.Setup where

import           Control.Applicative
import qualified Data.Attoparsec.Text as P
import           Data.Either.Unwrap
import qualified Data.Text            as T
import           Data.Yaml
import           Gli.Types
import           System.Process

setupProject :: String -> IO ()
setupProject file = do
  cfg <- decodeFile file :: IO (Maybe GliCfg)
  origin <- readProcess "git"
            ["config", "--get", "remote.origin.url"]
            ""
  let gitUrl = fromRight $ P.parseOnly parseGitUrl (T.pack origin)
  print gitUrl

parseGitUrl :: P.Parser GitUrl
parseGitUrl = do
  _ <- P.string "git@"
  d <- P.takeTill (':' ==)
  _ <- P.char ':'
  r <- P.takeTill ('\n' ==)
  return $ GitUrl d r
