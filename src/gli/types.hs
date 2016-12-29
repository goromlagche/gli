{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gli.Types where

import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import           Data.Map.Strict       as M
import qualified Data.Text             as T
import           GHC.Generics          (Generic)

data Setup =
  Setup { keyFile :: String } deriving (Show)

data Commands = CmdSetup Setup
              | CmdPrs
              deriving (Show)

data Project =
  Project { id              :: Int
          , description     :: Maybe T.Text
          , name            :: T.Text
          , ssh_url_to_repo :: T.Text
          } deriving (Generic)

instance Show Project where
   show (Project pid pdescription pname purl) =
       unlines [ "GitLab Project"
               , "Id:         " ++ show pid
               , "Name:       " ++ show pname
               , "Descripion: " ++ justOrEmpty pdescription
               , "Git Url:    " ++ show purl
               , ""
               ]

data MergeRequest = MergeRequest { id                        :: Int
                                 , title                     :: Maybe T.Text
                                 , iid                       :: Int
                                 , project_id                :: Int
                                 , description               :: Maybe T.Text
                                 , source_branch             :: T.Text
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
   show (MergeRequest mid mtitle _ _ mdescription msource_branch
         _ _ mauthor massignee _
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
             , "Branch:     " ++  show msource_branch
             , "Created At: " ++  show mcreated
             , "Updated At: " ++  show mupdated
             ]

data User = User { name     :: T.Text
                 , username :: T.Text
                 } deriving (Generic)

instance Show User where
   show (User _ uusername) = show uusername

data GliCfg = GliCfg { accounts :: Account
                     } deriving (Generic, Show)

newtype Account = Account (Map T.Text AccountConfig)
                deriving (Generic, Show)

data AccountConfig = AccountConfig { key :: String
                                   , url :: String
                                   } deriving (Generic, Show)

type GitlabAccountConfig = (String, B.ByteString)

data GitUrl = GitUrl { domain :: T.Text
                     , repo   :: T.Text
                     }

data LocalYmlContent = LocalYmlContent { masterFileConfig :: MasterFileConfig
                                       , project          :: Project
                                       } deriving (Generic, Show)

data MasterFileConfig = MasterFileConfig { file :: FilePath
                                         , key  :: T.Text
                                         }
                         deriving (Generic, Show)

instance Show GitUrl where
   show (GitUrl gdomain grepo) =
       unlines [ "Git Project Found"
               , "Domain:     " ++ show gdomain
               , "Repo:       " ++ show grepo
               ]

instance ToJSON MergeRequest  where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON User  where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Project where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Account where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON AccountConfig where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON LocalYmlContent where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON MasterFileConfig where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON MergeRequest
instance FromJSON User
instance FromJSON GliCfg
instance FromJSON Account
instance FromJSON AccountConfig
instance FromJSON Project
instance FromJSON LocalYmlContent
instance FromJSON MasterFileConfig


justOrEmpty :: Show a => Maybe a -> String
justOrEmpty (Just a)  = show a
justOrEmpty Nothing = ""

localYmlFile :: FilePath
localYmlFile = "gli.yml"

gitInfoExcludeFile :: FilePath
gitInfoExcludeFile = ".git/info/exclude"
