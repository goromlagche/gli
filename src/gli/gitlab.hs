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
  print project
  pRs <- mergeRequests configs $ id (project :: Project)
  putStrLn pRs
  putStrLn "/------------------------------------------------------------------------\\"

parseProject :: BLI.ByteString -> Maybe [Project]
parseProject body = decode body :: Maybe [Project]

mergeRequests :: (String -> AccountConfg) -> Int -> IO String
mergeRequests configs pid = do
  prResponseBody <- apiCall $
                    configs ( "/projects/" ++
                              show pid ++
                              "/merge_requests?state=opened")
  let body = justBody $ parseMergeRequest prResponseBody
  return (unlines (map show body))

parseMergeRequest :: BLI.ByteString -> Maybe [MergeRequest]
parseMergeRequest body = decode body :: Maybe [MergeRequest]

justBody :: Maybe [a] -> [a]
justBody Nothing = []
justBody (Just elems) = elems

accountConfigs :: Account -> String -> AccountConfg
accountConfigs x apiUrl =
  (T.unpack(url (x :: Account)) ++ apiUrl, B.pack $ T.unpack $ key x)
