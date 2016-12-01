{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gli.Types where

import           Data.Aeson
import qualified Data.Text    as T
import           GHC.Generics (Generic)

data Project =
  Project { id          :: Int
          , description :: Maybe T.Text
          , name        :: T.Text
          } deriving (Generic, Show)

data MergeRequest = MergeRequest { id                          :: Int
                                 , iid                         :: Int
                                 , project_id                  :: Int
                                 , title                       :: T.Text
                                 , description                 :: Maybe T.Text
                                 , state                       :: T.Text
                                 , created_at                  :: T.Text
                                 , updated_at                  :: T.Text
                                 , target_branch               :: T.Text
                                 , source_branch               :: T.Text
                                 , upvotes                     :: Int
                                 , downvotes                   :: Int
                                 , author                      :: User
                                 , assignee                    :: Maybe User
                                 , source_project_id           :: Int
                                 , target_project_id           :: Int
                                 , labels                      :: Maybe [T.Text]
                                 , work_in_progress            :: Bool
                                 , milestone                   :: Maybe T.Text
                                 , merge_when_build_succeeds   :: Bool
                                 , merge_status                :: T.Text
                                 , subscribed                  :: Bool
                                 , user_notes_count            :: Int
                                 , should_remove_source_branch :: Bool
                                 , force_remove_source_branch  :: Bool
                                 } deriving (Generic, Show)

data User = User { name     :: T.Text
                 , username :: T.Text
                 } deriving (Generic, Show)

data GliCfg = GliCfg { accounts :: [Account]
                     } deriving (Generic, Show)

data Account = Account { key :: T.Text
                       , url :: T.Text
                       } deriving (Generic, Show)

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
