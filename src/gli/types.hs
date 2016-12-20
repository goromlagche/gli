{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gli.Types where

import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T
import           GHC.Generics          (Generic)

data Setup =
  Setup { keyFile :: String } deriving (Show)

data Commands =
  Commands { setup :: Setup
           } deriving (Show)

data Project =
  Project { id          :: Int
          , description :: Maybe T.Text
          , name        :: T.Text
          } deriving (Generic)

instance Show Project where
   show (Project pid pdescription pname) =
       unlines [ ""
               , "Id:         " ++ show pid
               , "Name:       " ++ show pname
               , "Descripion: " ++ justOrEmpty pdescription
               , ""
               ]

data MergeRequest = MergeRequest { id                        :: Int
                                 , title                     :: Maybe T.Text
                                 , iid                       :: Int
                                 , project_id                :: Int
                                 , description               :: Maybe T.Text
                                 , upvotes                   :: Int
                                 , downvotes                 :: Int
                                 , author                    :: User
                                 , assignee                  :: Maybe User
                                 , source_project_id         :: Int
                                 , target_project_id         :: Int
                                 , labels                    :: [Maybe T.Text]
                                 , work_in_progress          :: Bool
                                 , milestone                 :: Maybe T.Text
                                 , merge_when_build_succeeds :: Bool
                                 , merge_status              :: T.Text
                                 , subscribed                :: Maybe Bool
                                 , web_url                   :: T.Text
                                 , sha                       :: T.Text
                                 , created_at                :: T.Text
                                 , updated_at                :: T.Text
                                 } deriving (Generic)

instance Show MergeRequest where
   show (MergeRequest mid mtitle _ _ mdescription _
         _ mauthor massignee _
         _ _ mwork_in_progress _
         _ mmerge_status _ mweb_url _ mcreated mupdated) =
     unlines [ "ID:         " ++  show mid
             , "URL:        " ++  show mweb_url
             , "Title:      " ++  justOrEmpty mtitle
             , "Descripion: " ++  justOrEmpty mdescription
             , "Author:     " ++  show mauthor
             , "Assignee:   " ++  justOrEmpty massignee
             , "WIP:        " ++  show mwork_in_progress
             , "Status:     " ++  show mmerge_status
             , "Created At: " ++  show mcreated
             , "Updated At: " ++  show mupdated
             , ""
             ]

data User = User { name     :: T.Text
                 , username :: T.Text
                 } deriving (Generic)

instance Show User where
   show (User _ uusername) = show uusername

data GliCfg = GliCfg { accounts :: [Account]
                     } deriving (Generic, Show)

data Account = Account { key :: T.Text
                       , url :: T.Text
                       } deriving (Generic, Show)

type AccountConfg = (String, B.ByteString)

instance ToJSON MergeRequest  where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON User  where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Project where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON MergeRequest
instance FromJSON User
instance FromJSON GliCfg
instance FromJSON Account
instance FromJSON Project

justOrEmpty :: Show a => Maybe a -> String
justOrEmpty (Just a)  = show a
justOrEmpty Nothing = ""
