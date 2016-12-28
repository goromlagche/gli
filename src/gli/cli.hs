{-# LANGUAGE DuplicateRecordFields #-}

module Gli.Cli where

import qualified Data.Map.Strict     as M
import           Data.Maybe
import qualified Data.Yaml           as Y
import           Gli.Setup
import           Gli.Types
import           Options.Applicative

opts :: ParserInfo Commands
opts = info (helper <*> versionOption <*> commands)
      ( fullDesc
     <> progDesc "Tiny git\"lab/hub\" cli wrapper"
     <> header "gli - make PR review easy again" )
  where
    versionOption = infoOption "Version 0.0.1"
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
runCli (CmdPrs) = do
  localCfg <- Y.decodeFile localYmlFile :: IO (Maybe LocalYmlContent)
  let masterFileKey = masterFileConfig $ fromJust localCfg
  masterCfg <- Y.decodeFile (file masterFileKey) :: IO (Maybe GliCfg)
  case masterCfg of
    Nothing -> putStrLn $ mappend "Unable to parse file " (show $ file masterFileKey)
    Just b  -> print $ M.lookup (key (masterFileKey :: MasterFileConfig)) (accountMap (accounts b))


runParser :: IO ()
runParser = execParser opts >>= runCli
