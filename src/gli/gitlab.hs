{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Gli.Gitlab where

import           Data.Aeson
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Internal      as BI
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.Text                     as T
import           Gli.Types
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple
import           Prelude                       hiding (id)

apiCall :: String -> String -> IO (BLI.ByteString)
apiCall apiUrl apiKey = do
  manager <- newManager tlsManagerSettings
  request' <- parseRequest apiUrl
  let request = setRequestManager manager
                $ setRequestHeader "PRIVATE-TOKEN" [B.pack apiKey]
                $ request'

  response <- httpLBS request
  return (getResponseBody response)

projects :: GliCfg -> IO ()
projects cfg =  project (accounts cfg)
  where
    project :: [Account] -> IO ()
    project (x:xs) = do
      let apiUrl = T.unpack(url (x :: Account))
          apiKey = T.unpack $ key x
      projectResponseBody <- apiCall (apiUrl ++ "/projects") apiKey
      let body = justBody $ parseProject projectResponseBody
      putStrLn $ T.unpack $ T.unlines (map printProject body)
      project xs
    project [] = return ()

parseProject :: BLI.ByteString -> Maybe [Project]
parseProject body = decode body :: Maybe [Project]

printProject :: Project -> T.Text
printProject project =
  T.unlines [ ""
            , T.concat ["NAME:       ", name (project :: Project)]
            , T.concat ["DESCRIPION: ", justOrEmpty $ description (project :: Project)]
            , ""]

mergeRequests :: String -> String -> IO ()
mergeRequests apiUrl apiKey = do
  prResponseBody <- apiCall apiUrl apiKey
  let body = justBody $ parseMergeRequest prResponseBody
  print $ T.unlines (map printMergeRequest body)

parseMergeRequest :: BLI.ByteString -> Maybe [MergeRequest]
parseMergeRequest body = decode body :: Maybe [MergeRequest]

printMergeRequest :: MergeRequest -> T.Text
printMergeRequest pr =
  T.unlines [ T.concat ["NAME:       ", title pr]
            , T.concat ["DESCRIPION: ", justOrEmpty $ description (pr :: MergeRequest)]
            , ""]

justOrEmpty :: Maybe T.Text -> T.Text
justOrEmpty (Just a)  = a
justOrEmpty Nothing = ""

justBody :: Maybe [a] -> [a]
justBody Nothing = []
justBody (Just elems) = elems
