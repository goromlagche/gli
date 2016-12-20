module Gli.Cli where

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
                    (info setupProject
                      ( fullDesc
                        <> progDesc "Setup you project" )))

setupProject :: Parser Setup
setupProject = Setup
     <$> strOption
         ( long "file"
        <> short 'f'
        <> metavar "FILE"
        <> help "Accepts input file which has gitlab keys" )

runCli :: Commands -> IO ()
runCli cmd = putStrLn $ (keyFile . setup) cmd

runParser :: IO Commands
runParser = execParser opts
