{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Gli.Gitlab where

import           Data.Aeson
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.Text                     as T
import           Data.Time.Format.Human        (humanReadableTime)
import           Data.Time.LocalTime
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
                [B.pack $ key (accountConfig :: AccountConfig)]
                $ request'

  response <- httpLBS request
  return (getResponseBody response)

getProject :: T.Text -> AccountConfig -> IO Project
getProject repoUrl a = do
  projectResponseBody <-
    apiCall (AccountConfig (key (a :: AccountConfig)) (url a ++ "/projects"))
  case parseProject projectResponseBody of
    Just projects ->
      return (head $ filter (\p -> ssh_url_to_repo p == repoUrl) projects)
    Nothing -> error "Unable to fetch projects"

parseProject :: BLI.ByteString -> Maybe [Project]
parseProject body = decode body :: Maybe [Project]

mergeRequests :: AccountConfig -> IO ()
mergeRequests cfg = do
  prResponseBody <- apiCall mcfg
  let body = justBody $ parseMergeRequest prResponseBody
  mapM_ (modifyAndShow cfg) body
  where
    mcfg = AccountConfig (key (cfg :: AccountConfig))
           (url cfg ++ "/merge_requests?state=opened")

modifyAndShow :: AccountConfig -> MergeRequest -> IO ()
modifyAndShow cfg m = do
  c  <- humanReadableTime $ created_at (m :: MergeRequest)
  u  <- humanReadableTime $ updated_at (m :: MergeRequest)
  b  <- builds bc
  putStrLn $
    unlines (lines (show m)
              ++ [ "Created At:      " ++ show c
                 , "Updated At:      " ++ show u
                 , "Builds:"])
  mapM_ (printBuild (web_url m) (id (m :: MergeRequest))) b
  where
    bc = AccountConfig (key (cfg :: AccountConfig))
         (url cfg ++ "/repository/commits/" ++ T.unpack (sha m) ++ "/builds")

parseMergeRequest :: BLI.ByteString -> Maybe [MergeRequest]
parseMergeRequest body = decode body :: Maybe [MergeRequest]

printBuild :: T.Text -> Int -> Build -> IO ()
printBuild u i b = do
  c <- utcToLocalZonedTime $ created_at  (b :: Build)
  f <- finished_time (finished_at b)
  putStrLn (unlines (lines (show b)
                   ++ [ "    Url:             " ++ bUrl
                      , "    Created At:      " ++ show c
                      , "    Finished At:     " ++ f]))
  where
    bUrl = T.unpack (T.replace
                     (T.pack("/merge_requests/" ++ show i)) (T.pack "") u)
           ++ "/builds/"
           ++ show (id (b :: Build))

    finished_time fa = case fa of
                         Just fa -> do
                           lf <- utcToLocalZonedTime fa
                           return (show $ lf)
                         Nothing -> return "Pending"

builds :: AccountConfig -> IO [Build]
builds cfg = do
  pResponseBody <- apiCall cfg
  return (justBody $ parseBuilds pResponseBody)

parseBuilds :: BLI.ByteString -> Maybe [Build]
parseBuilds body = decode body :: Maybe [Build]

justBody :: Maybe [a] -> [a]
justBody Nothing = []
justBody (Just elems) = elems
