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


-- fetchAccount :: AccountConfig -> IO ()
-- fetchAccount x = do
--   let configs = accountConfigs x
--   accountResponseBody <- apiCall $ configs "/projects"
--   let body = justBody $ parseProject accountResponseBody
--   mapM_ (printProject configs) body

-- printProject :: (String -> AccountConfg) -> Project -> IO ()
-- printProject configs project = do
--   putStrLn "\\------------------------------------------------------------------------/"
--   print project
--   pRs <- mergeRequests configs $ id (project :: Project)
--   putStrLn pRs
--   putStrLn "/------------------------------------------------------------------------\\"

-- parseProject :: BLI.ByteString -> Maybe [Project]
-- parseProject body = decode body :: Maybe [Project]

-- mergeRequests :: (String -> AccountConfg) -> Int -> IO String
-- mergeRequests configs pid = do
--   prResponseBody <- apiCall $
--                     configs ( "/projects/" ++
--                               show pid ++
--                               "/merge_requests?state=opened")
--   let body = justBody $ parseMergeRequest prResponseBody
--   return (unlines (map show body))

-- parseMergeRequest :: BLI.ByteString -> Maybe [MergeRequest]
-- parseMergeRequest body = decode body :: Maybe [MergeRequest]

-- justBody :: Maybe [a] -> [a]
-- justBody Nothing = []
-- justBody (Just elems) = elems

-- accountConfigs :: Account -> String -> AccountConfg
-- accountConfigs x apiUrl =
--   (T.unpack(url (x :: Account)) ++ apiUrl, B.pack $ T.unpack $ key x)
