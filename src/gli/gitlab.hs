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

apiCall :: AccountConfig -> IO BLI.ByteString
apiCall accountConfig = do
  manager <- newManager tlsManagerSettings
  request' <- parseRequest (url accountConfig)
  let request = setRequestManager manager
                $ setRequestHeader "PRIVATE-TOKEN"
                [(B.pack $ key (accountConfig :: AccountConfig))]
                $ request'

  response <- httpLBS request
  return (getResponseBody response)

getProject :: T.Text -> AccountConfig -> IO Project
getProject repoUrl a = do
  projectResponseBody <-
    apiCall (AccountConfig (key (a :: AccountConfig)) (url a ++ "/projects"))
  case parseProject projectResponseBody of
    Just projects -> do
      let project = head $ filter (\p -> ssh_url_to_repo p == repoUrl) projects
      return (project)
    Nothing -> error "Unable to fetch projects"

parseProject :: BLI.ByteString -> Maybe [Project]
parseProject body = decode body :: Maybe [Project]

mergeRequests :: AccountConfig -> IO String
mergeRequests cfg = do
  prResponseBody <- apiCall cfg
  let body = justBody $ parseMergeRequest prResponseBody
  return (unlines (map show body))

parseMergeRequest :: BLI.ByteString -> Maybe [MergeRequest]
parseMergeRequest body = decode body :: Maybe [MergeRequest]

justBody :: Maybe [a] -> [a]
justBody Nothing = []
justBody (Just elems) = elems
