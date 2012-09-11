
{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Maybe
import Data.Tuple.Curry

data Host = Host { host_id :: Int
                 , hw_address :: String
                 , host_profile_id :: Maybe Int
                 , host_profile_name :: Maybe String
                 }

hostFromTuple :: (Int, String, Maybe Int, Maybe String) -> Host
hostFromTuple = uncurryN Host

instance ToJSON Host where
  toJSON (Host host_id' hw_address' host_profile_id' host_profile_name') = 
    object [ "host_id"           .= host_id'
           , "hw_address"        .= hw_address'
           , "host_profile_id"   .= fromMaybe (-1) host_profile_id'
           , "host_profile_name" .= fromMaybe "unassigned" host_profile_name'
           ]

data Profile = Profile { profile_id :: Int
                       , profile_name :: String
                       } deriving (Show)

profileFromTuple :: (Int, String) -> Profile
profileFromTuple = uncurryN Profile

instance ToJSON Profile where
  toJSON (Profile profile_id' profile_name') =
    object [ "profile_id"   .= profile_id'
           , "profile_name" .= profile_name'
           ]

data Disk = Disk { disk_id :: Int
                 , disk_name :: String
                 } deriving (Show)

diskFromTuple :: (Int, String) -> Disk
diskFromTuple = uncurryN Disk

instance ToJSON Disk where
  toJSON (Disk disk_id' disk_name') =
    object [ "disk_id"   .= disk_id'
           , "disk_name" .= disk_name'
           ]

data Partition = Partition { partition_id :: Int
                           , partition_number :: Int
                           , partition_type :: Int
                           , size_in_mb :: Int
                           } deriving (Show)

partitionFromTuple :: (Int, Int, Int, Int) -> Partition
partitionFromTuple = uncurryN Partition

instance ToJSON Partition where
  toJSON (Partition partition_id'
                    partition_number'
                    partition_type'
                    size_in_mb') =
    object [ "partition_id"     .= partition_id'
           , "partition_number" .= partition_number'
           , "partition_type"   .= partition_type'
           , "size_in_mb"       .= size_in_mb'
           ]
