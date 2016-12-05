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

apiCall :: AccountConfg -> IO BLI.ByteString
apiCall accountConfig = do
  manager <- newManager tlsManagerSettings
  request' <- parseRequest (fst accountConfig)
  let request = setRequestManager manager
                $ setRequestHeader "PRIVATE-TOKEN" [(snd accountConfig)]
                $ request'

  response <- httpLBS request
  return (getResponseBody response)

allAccounts :: GliCfg -> IO ()
allAccounts cfg =  mapM_ account (accounts cfg)

account :: Account -> IO ()
account x = do
  let configs = accountConfigs x
  accountResponseBody <- apiCall $ configs "/projects"
  let body = justBody $ parseProject accountResponseBody
  mapM_ (printProject configs) body

printProject :: (String -> AccountConfg) -> Project -> IO ()
printProject configs project = do
  putStrLn "\\------------------------------------------------------------------------/"
  putStrLn $ T.unpack $ projectData project
  pRs <- mergeRequests configs $ id (project :: Project)
  putStrLn $ T.unpack pRs
  putStrLn "/------------------------------------------------------------------------\\"

parseProject :: BLI.ByteString -> Maybe [Project]
parseProject body = decode body :: Maybe [Project]

projectData :: Project -> T.Text
projectData project =
  T.unlines [ ""
            , T.concat ["Name:       ", name (project :: Project)]
            , T.concat ["Descripion: ", justOrEmpty $ description (project :: Project)]
            , ""]

mergeRequests :: (String -> AccountConfg) -> Int -> IO T.Text
mergeRequests configs pid = do
  prResponseBody <- apiCall $
                    configs ( "/projects/" ++
                              show pid ++
                              "/merge_requests?state=opened")
  let body = justBody $ parseMergeRequest prResponseBody
  return (T.unlines (map printMergeRequest body))

parseMergeRequest :: BLI.ByteString -> Maybe [MergeRequest]
parseMergeRequest body = decode body :: Maybe [MergeRequest]

printMergeRequest :: MergeRequest -> T.Text
printMergeRequest pr =
  T.unlines [ T.concat ["URL:        ", web_url pr]
            , T.concat ["Title:      ", justOrEmpty $ title pr]
            , T.concat ["Descripion: ", justOrEmpty $ description (pr :: MergeRequest)]
            , T.concat ["Author:     ", (username . author) pr]
            , ""]

justOrEmpty :: Maybe T.Text -> T.Text
justOrEmpty (Just a)  = a
justOrEmpty Nothing = ""

justBody :: Maybe [a] -> [a]
justBody Nothing = []
justBody (Just elems) = elems

accountConfigs :: Account -> String -> AccountConfg
accountConfigs x apiUrl =
  (T.unpack(url (x :: Account)) ++ apiUrl, B.pack $ T.unpack $ key x)
