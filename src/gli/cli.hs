module Gli.Cli where

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
commands = Commands
     <$> subparser (command "setup"
                    (info (helper <*> parseSetupFile)
                      ( fullDesc
                        <> progDesc "Setup you project" )))

parseSetupFile :: Parser Setup
parseSetupFile = Setup
     <$> strOption
         ( long "file"
        <> short 'f'
        <> metavar "FILE"
        <> help "Accepts input file which has gitlab keys" )

runCli :: Commands -> IO ()
runCli cmd = setupProject $ (keyFile . setup) cmd

runParser :: IO ()
runParser = execParser opts >>= runCli
