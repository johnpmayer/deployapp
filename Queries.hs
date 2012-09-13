
{-# Options -Wall #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Queries where

import Control.Applicative

import Database.HDBC
import Database.HDBC.ODBC
import Database.MetaHDBC

import Types
import Utils

checkHostQuery :: String -> IO (Maybe Int)
checkHostQuery mac =
  do
    runQuery $(compileQuery $ unlines
              [ "select id from host"
              , "where hw_address = ?mac;"
              ])
     >>= (\results -> 
      case results of
        [host] -> return $ Just host
        _      -> return Nothing)

registerHostQuery :: String -> IO Integer
registerHostQuery mac =
  do
    runQuery $(compileQuery $ unlines
              [ "insert into host"
              , "(hw_address, profile_id)"
              , "values (?mac, null);"
              ])
    

hostsQuery :: IO [Host]
hostsQuery = 
  do    
    map hostFromTuple <$> runQuery $(compileQuery $ unlines
              [ "select"
              , "  h.id"
              , ", h.hw_address"
              , ", p.id"
              , ", p.name"
              , "from host h"
              , "left outer join profile p"
              , "on h.profile_id = p.id;"
              ])
    

updateHostProfileQuery :: Int -> Int -> IO Integer
updateHostProfileQuery host profile =
  do
    runQuery $(compileQuery $ unlines
              [ "update host"
              , "set profile_id = ?profile"
              , "where id = ?host;"
              ])
    

unassignHostProfileQuery :: Int -> IO Integer
unassignHostProfileQuery host =
  do
    runQuery $(compileQuery $ unlines
              [ "update host"
              , "set profile_id = null"
              , "where id = ?host;"
              ])
    

profilesQuery :: IO [Profile]
profilesQuery = 
  do 
    map profileFromTuple <$> runQuery $(compileQuery $ unlines
              [ "select"
              , "  id"
              , ", name"
              , ", description"
              , ", disk_id"
              , "from profile;"
              ])
    

newProfileQuery :: String -> String -> Int -> IO Integer
newProfileQuery name desc disk =
  do
    runQuery $(compileQuery $ unlines
              [ "insert into profile"
              , "(name, description, disk_id)"
              , "values"
              , "(?name, ?desc, ?disk);"
              ])
    

deleteProfileQuery :: Int -> IO Integer
deleteProfileQuery profile =
  do
    runQuery $(compileQuery $
              "delete from profile where id = ?profile;")
    

disksQuery :: IO [Disk]
disksQuery =
  do
    map diskFromTuple <$> runQuery $(compileQuery $ unlines
              [ "select"
              ,  "  id"
              ,  ", name"
              ,  "from disk;"
              ])
    

newDiskQuery :: String -> IO Integer
newDiskQuery diskname =
  do
    runQuery $(compileQuery $ 
              "insert into disk (name) values (?diskname);")
    

deleteDiskQuery :: Int -> IO Integer
deleteDiskQuery disk =
  do
    runQuery $(compileQuery $
              "delete from disk where id = ?disk;")
    
    
diskPartitionsQuery :: Int -> IO [Partition]
diskPartitionsQuery disk =
  do
    map partitionFromTuple <$> runQuery $(compileQuery $ unlines
              [ "select"
              , "  id"
              , ", partition_number"
              , ", partition_type"
              , ", size_in_mb"
              , "from disk_partition"
              , "where disk_id = ?disk;"
              ])
    

newPrimaryPartitionQuery :: Int -> Int -> Int -> IO Integer
newPrimaryPartitionQuery disk number size =
  do
    runQuery $(compileQuery $ unlines
              [ "insert into disk_partition"
              , "( disk_id"
              , ", partition_number"
              , ", partition_type"
              , ", size_in_mb"
              , ", chain_partition_number"
              , ", chain_partition_type"
              , ") values (?disk, ?number, ?number"
              , "         , ?size, null, null);"
              ])
    
    
newExtendedPartitionQuery :: Int -> Int -> Int -> IO Integer
newExtendedPartitionQuery disk number size =
  do
    runQuery $(compileQuery $ unlines
              [ "insert into disk_partition"
              , "( disk_id"
              , ", partition_number"
              , ", partition_type"
              , ", size_in_mb"
              , ", chain_partition_number"
              , ", chain_partition_type"
              , ") values (?disk, ?number, 0, ?size, null, null);"
              ])
    
    
newLogicalPartitionQuery :: Int -> Int -> Int -> IO Integer
newLogicalPartitionQuery disk number size =
  do
    let parttype = number
    let (expartn, expartt) = if number==5 
                             then (0,0)
                             else (number-1,number-1)
    runQuery $(compileQuery $ unlines
              [ "insert into disk_partition"
              , "( disk_id"
              , ", partition_number"
              , ", partition_type"
              , ", size_in_mb"
              , ", chain_partition_number"
              , ", chain_partition_type"
              , ")"
              , "values"
              , "( ?disk, ?number, ?number, ?size"
              , ", max(?expartn,(select dp.partition_number"    
              , "  from disk_partition dp"
              , "  where dp.disk_id = ?disk"
              , "  and dp.partition_type = 0))"
              , ", ?expartt);" 
              ])
    

deletePartitionQuery :: Int -> IO Integer
deletePartitionQuery partitionid =
  do
    runQuery $(compileQuery $ unlines
              [ "delete from disk_partition"
              , "where id = ?partitionid"
              ])
    

