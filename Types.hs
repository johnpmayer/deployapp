
{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.List
import Data.Maybe
import Data.Tuple.Curry

data Host = Host { host_id          :: Int
                 , hw_address       :: String
                 , host_profile_id  :: Maybe Int
                 , last_reported_ip :: Maybe Int
                 , ip_assignment    :: Maybe Int
                 } deriving (Show)

hostFromTuple :: (Int, String, Maybe Int, Maybe Int, Maybe Int) 
              -> Host
hostFromTuple = uncurryN Host

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
                       } deriving (Show)

profileFromTuple :: (Int, String, String, Int) -> Profile
profileFromTuple = uncurryN Profile

instance ToJSON Profile where
  toJSON (Profile profile_id' 
                  profile_name'
                  profile_desc'
                  profile_disk') =
    object [ "id"          .= profile_id'
           , "name"        .= profile_name'
           , "description" .= profile_desc'
           , "disk_id"     .= profile_disk'
           ]

data Disk = Disk { disk_id :: Int
                 , disk_name :: String
                 } deriving (Show)

diskFromTuple :: (Int, String) -> Disk
diskFromTuple = uncurryN Disk

instance ToJSON Disk where
  toJSON (Disk disk_id' disk_name') =
    object [ "id"   .= disk_id'
           , "name" .= disk_name'
           ]

data Partition = Partition { partition_id :: Int
                           , partition_number :: Int
                           , partition_type :: Int
                           , size_in_mb :: Int
                           } deriving (Show, Eq, Ord)

partitionFromTuple :: (Int, Int, Int, Int) -> Partition
partitionFromTuple = uncurryN Partition

instance ToJSON Partition where
  toJSON (Partition partition_id'
                    partition_number'
                    partition_type'
                    size_in_mb') =
    object [ "id"         .= partition_id'
           , "number"     .= partition_number'
           , "type"       .= partition_type'
           , "size_in_mb" .= size_in_mb'
           ]

fdiskEntry :: Partition -> String
fdiskEntry (Partition _id number' type' size') =
  concat . intersperse ":" . map show $ [number',type',size']
