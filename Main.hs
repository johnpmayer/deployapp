
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
    route [ ("app/hosts", method GET $ 
                          makeJSONHandler hostsQuery)
          , ("app/profiles", method GET    
                             $ makeJSONHandler profilesQuery)
          , ("app/profile", method PUT    
                            createProfileHandler)
          , ("app/disks", method GET    
                          $ makeJSONHandler disksQuery)
          , ("app/disk", method PUT    
                         createDiskHandler)
          , ("app/disk", method DELETE  
                         deleteDiskHandler)
          , ("app/partitions/:disk_id", method GET    
                                        getPartitionsHandler)
          , ("app/partition", method PUT    
                              createPartitionHandler)
          , ("app/partition", method DELETE 
                              deletePartitionHandler)
          , ("core/check/:mac", method GET    
                                checkHostHandler)
          , ("core/register/:mac", method GET    
                                   registerHostHandler)
          , ("core/setup/:host_id", method GET    
                                setupHostHandler)
          ]

{- APP -}

createProfileHandler :: Snap ()
createProfileHandler =
  do
    name <- requireString "name"
    desc <- requireString "description"
    disk <- requireInt "disk_id"
    makeJSONHandler $ newProfileQuery name desc disk

createDiskHandler :: Snap ()
createDiskHandler =
  do
    disk_name <- requireString "disk_name"
    makeJSONHandler $ newDiskQuery disk_name

deleteDiskHandler :: Snap ()
deleteDiskHandler =
  do
    disk_id <- requireInt "disk_id"
    makeJSONHandler $ deleteDiskQuery disk_id

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
    case partition_type of
      "Primary"  -> makeJSONHandler $ 
                    newPrimaryPartitionQuery  disk number size
      "Extended" -> makeJSONHandler $ 
                    newExtendedPartitionQuery disk number size
      "Logical"  -> makeJSONHandler $ 
                    newLogicalPartitionQuery  disk number size
      _          -> pass

deletePartitionHandler :: Snap ()
deletePartitionHandler =
  do
    partition <- requireInt "partition_id"
    makeJSONHandler $ deletePartitionQuery partition

{- CORE -}

checkHostHandler :: Snap ()
checkHostHandler =
  do
    mac <- requireString "mac"
    makeJSONHandler $ checkHostQuery mac

registerHostHandler :: Snap ()
registerHostHandler =
  do
    mac <- requireString "mac"
    makeJSONHandler $ registerHostQuery mac

setupHostHandler :: Snap ()
setupHostHandler =
  do
    mbs <- getParam "host_id"
    case mbs of     
      (Just bs) -> writeBS bs
      Nothing   -> pass
