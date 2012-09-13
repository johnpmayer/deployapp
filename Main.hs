
{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Main where

import           Control.Applicative

--import qualified Data.ByteString.Char8 as B

--import           Data.Tuple.Sequence

import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

import           Queries
import           Utils

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (serveFile "index.html") <|>
    dir "static" (serveDirectory "static") <|>
    route [ ("app/hosts", method GET $ makeJSONHandler hostsQuery)
          , ("app/host/profile", method POST updateHostProfile <|>
                                 method DELETE unassignHostProfile)

          , ("app/profiles", method GET $ makeJSONHandler profilesQuery)
          , ("app/profile", method PUT createProfile <|>
                            method DELETE deleteProfile)

          , ("app/disks", method GET $ makeJSONHandler disksQuery)
          , ("app/disk", method PUT createDisk <|>
                         method DELETE deleteDisk)

          , ("app/partitions/:disk_id", method GET getPartitions)
          , ("app/partition", method PUT createPartition <|>
                              method DELETE deletePartition)

          , ("core/check/:mac", method GET checkHost)
          , ("core/register/:mac", method GET registerHost)
          , ("core/setup/:host_id", method GET setupHost)
          ]

{- APP -}

updateHostProfile :: Snap ()
updateHostProfile = 
  do
    host <- requireInt "host_id"
    profile <- requireInt "profile_id"
    makeJSONHandler $ updateHostProfileQuery host profile

unassignHostProfile :: Snap ()
unassignHostProfile =
  do
    host <- requireInt "host_id"
    makeJSONHandler $ unassignHostProfileQuery host

createProfile :: Snap ()
createProfile =
  do
    name <- requireString "name"
    desc <- requireString "description"
    disk <- requireInt "disk_id"
    makeJSONHandler $ newProfileQuery name desc disk

deleteProfile :: Snap ()
deleteProfile =
  do
    profile_id <- requireInt "profile_id"
    makeJSONHandler $ deleteProfileQuery profile_id

createDisk :: Snap ()
createDisk =
  do
    disk_name <- requireString "disk_name"
    makeJSONHandler $ newDiskQuery disk_name

deleteDisk :: Snap ()
deleteDisk =
  do
    disk_id <- requireInt "disk_id"
    makeJSONHandler $ deleteDiskQuery disk_id

getPartitions :: Snap ()
getPartitions = 
  do
    disk_id <- requireInt "disk_id"
    makeJSONHandler $ diskPartitionsQuery disk_id

createPartition :: Snap ()
createPartition =
  do
    disk <- requireInt "disk_id"
    number <- requireInt "partition_number"
    partition_type <- requireString "partition_type"
    size <- requireInt "size_in_mb"
    case partition_type of
      "Primary"  -> makeJSONHandler $ 
                    newPrimaryPartitionQuery  disk number size
      "Extended" -> makeJSONHandler $ 
                    newExtendedPartitionQuery disk number size
      "Logical"  -> makeJSONHandler $ 
                    newLogicalPartitionQuery  disk number size
      _          -> pass

deletePartition :: Snap ()
deletePartition =
  do
    partition <- requireInt "partition_id"
    makeJSONHandler $ deletePartitionQuery partition

{- CORE -}

checkHost :: Snap ()
checkHost =
  do
    mac <- requireString "mac"
    makeJSONHandler $ checkHostQuery mac

registerHost :: Snap ()
registerHost =
  do
    mac <- requireString "mac"
    makeJSONHandler $ registerHostQuery mac

setupHost :: Snap ()
setupHost =
  do
    mbs <- getParam "host_id"
    case mbs of     
      (Just bs) -> writeBS bs
      Nothing   -> pass
