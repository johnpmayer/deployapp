
{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.List
import Data.Maybe
--import Data.Tuple.Curry

data Host = Host { host_id          :: Int
                 , hw_address       :: String
                 , host_profile_id  :: Maybe Int
                 , last_reported_ip :: Maybe Int
                 , ip_assignment    :: Maybe Int
                 } deriving (Show)

instance ToJSON Host where
  toJSON (Host host_id' hw_address' profile' last_ip' ip_assign') = 
    object [ "id"               .= host_id'
           , "hw_address"       .= hw_address'
           , "profile_id"       .= fromMaybe (-1) profile'
           , "last_reported_ip" .= fromMaybe (-1) last_ip'
           , "ip_assignment"    .= fromMaybe (-1) ip_assign'
           ]

data Profile = Profile { profile_id :: Int
                       , profile_name :: String
                       , profile_description :: String
                       , profile_disk_id :: Int
                       , profile_image_id :: Int
                       } deriving (Show)

instance ToJSON Profile where
  toJSON (Profile profile_id' 
                  profile_name'
                  profile_desc'
                  profile_disk'
                  profile_image_id') =
    object [ "id"                .= profile_id'
           , "name"              .= profile_name'
           , "description"       .= profile_desc'
           , "disk_id"           .= profile_disk'
           , "image_id"          .= profile_image_id'
           ]

data Image = Image { image_id :: Int
                   , image_name :: String
                   , image_archive_url :: String
                   } deriving (Show)

instance ToJSON Image where
  toJSON (Image id' name' archive_url') =
    object [ "id"          .= id'
           , "name"        .= name'
           , "archive_url" .= archive_url'
           ]

data Software = Software { software_id :: Int
                         , software_package_name :: String
                         }

instance ToJSON Software where
  toJSON (Software id' package_name') =
    object [ "id"           .= id'
           , "package_name" .= package_name'
           ]

data Disk = Disk { disk_id :: Int
                 , disk_name :: String
                 } deriving (Show)

instance ToJSON Disk where
  toJSON (Disk disk_id' disk_name') =
    object [ "id"   .= disk_id'
           , "name" .= disk_name'
           ]

data Partition = Partition { partition_id     :: Int
                           , partition_number :: Int
                           , partition_type   :: Int
                           , mount_point      :: String
                           , is_boot          :: Int
                           , size_in_mb       :: Int
                           } deriving (Show, Eq, Ord)

instance ToJSON Partition where
  toJSON (Partition partition_id'
                    partition_number'
                    partition_type'
                    mount_point'
                    is_boot'
                    size_in_mb') =
    object [ "id"          .= partition_id'
           , "number"      .= partition_number'
           , "type"        .= partition_type'
           , "mount_point" .= mount_point'
           , "is_boot"     .= is_boot'
           , "size_in_mb"  .= size_in_mb'
           ]

fdiskEntry :: Partition -> String
fdiskEntry (Partition _id number' type' mount_point' is_boot' size') =
  concat . intersperse ":" $ (map show [number',type',size']) 
                           ++ [mount_point'] ++ [show is_boot']
