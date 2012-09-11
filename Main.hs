
{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Main where

import           Control.Applicative

--import qualified Data.ByteString.Char8 as B

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
    route [ ("app/get/hosts", method GET $ makeJSONHandler hostsQuery)
          , ("app/get/profiles", method GET $ makeJSONHandler profilesQuery)
          , ("app/get/disks", method GET $ makeJSONHandler disksQuery)
          , ("app/put/disk", method PUT createDiskHandler)
          , ("app/delete/disk", method DELETE deleteDiskHandler)
          , ("app/get/partitions/:disk_id", method GET $ getPartitionsHandler)
          , ("app/put/partition", method PUT $ createPartitionHandler)
          , ("app/delete/partition", method DELETE $ deletePartitionHandler)
          , ("app/setup/:mac", method GET $ setupHandler)
          ]

createDiskHandler :: Snap ()
createDiskHandler =
  do
    disk_name <- requireString "disk_name"
    makeJSONHandler . newDiskQuery $ disk_name

deleteDiskHandler :: Snap ()
deleteDiskHandler =
  do
    disk_id <- requireInt "disk_id"
    makeJSONHandler . deleteDiskQuery $ disk_id

getPartitionsHandler :: Snap ()
getPartitionsHandler = 
  do
    disk_id <- requireInt "disk_id"
    makeJSONHandler $ diskPartitionsQuery disk_id

createPartitionHandler :: Snap ()
createPartitionHandler =
  do
    disk <- requireInt "disk_id"
    number <- requireInt "partition_number"
    partition_type <- requireString "partition_type"
    size <- requireInt "size_in_mb"
    let mjh = makeJSONHandler
    case partition_type of
      "Primary"  -> mjh $ 
                    createPrimaryPartitionQuery  disk number size
      "Extended" -> mjh $ 
                    createExtendedPartitionQuery disk number size
      "Logical"  -> mjh $ 
                    createLogicalPartitionQuery  disk number size
      _          -> pass

deletePartitionHandler :: Snap ()
deletePartitionHandler =
  do
    partition <- requireInt "partition_id"
    makeJSONHandler $ deletePartitionQuery partition

setupHandler :: Snap ()
setupHandler =
  do
    mbs <- getParam "mac"
    case mbs of
      (Just bs) -> writeBS bs
      Nothing   -> pass
