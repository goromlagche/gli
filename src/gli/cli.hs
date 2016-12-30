{-# LANGUAGE DuplicateRecordFields #-}

module Gli.Cli where

import qualified Data.Map.Strict     as M
import           Data.Maybe
import qualified Data.Text           as T
import qualified Data.Yaml           as Y
import           Gli.Gitlab
import           Gli.Setup
import           Gli.Types
import           Options.Applicative
import           Prelude             hiding (id)

opts :: ParserInfo Commands
opts = info (helper <*> versionOption <*> commands)
      ( fullDesc
     <> progDesc "Tiny git\"lab/hub\" cli wrapper"
     <> header "gli - make PR review easy again" )
  where
    versionOption = infoOption "Version 0.0.1-alpha"
                    ( long "Version"
                      <> short 'v'
                      <> help "Show version")

commands :: Parser Commands
commands = subparser ((command "setup"
                          (info (helper <*> (CmdSetup <$> parseSetupFile))
                           ( fullDesc
                             <> progDesc "Setup you project" )))
                         <> (command "prs"
                             (info (pure CmdPrs)
                              ( fullDesc
                                <> progDesc "Get all PR info" ))))

parseSetupFile :: Parser Setup
parseSetupFile = Setup
     <$> strOption
         ( long "file"
        <> short 'f'
        <> metavar "FILE"
        <> help "Accepts input file which has gitlab keys" )

runCli :: Commands -> IO ()
runCli (CmdSetup cmd) = setupProject $ keyFile cmd
runCli CmdPrs = do
  localCfg <- Y.decodeFile localYmlFile :: IO (Maybe LocalYmlContent)
  let masterFileKey = masterFileConfig $ fromJust localCfg
  let filePath = file masterFileKey
  let fileKey = key (masterFileKey :: MasterFileConfig)
  let projectID = id (project (fromJust localCfg) :: Project)
  masterCfg <- Y.decodeFile filePath :: IO (Maybe GliCfg)
  case masterCfg of
    Nothing -> error $ mappend "Unable to parse file " (show filePath)
    Just b  ->
      case M.lookup fileKey (accountMap (accounts b)) of
        Nothing -> error $ concat[ "Unable to find key"
                                 , T.unpack fileKey
                                 , " in masterfile "
                                 , filePath]
        Just c  -> do
          mergeRequests (AccountConfig (key (c :: AccountConfig)) gitlabUrl)
          where
            gitlabUrl =
              url c
              ++ "/projects/"
              ++ show projectID
              ++ "/merge_requests?state=opened"

runParser :: IO ()
runParser = execParser opts >>= runCli
