
{-# Options -Wall #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Queries where

import Control.Applicative

import Data.List

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

reportHostIPQuery :: Int -> Int -> IO Integer
reportHostIPQuery host ip =
  do
    runQuery $(compileQuery $ unlines
             [ "update host"
             , "set last_reported_ip = ?ip"
             , "where id = ?host"
             ])

{-
availableIPsQuery :: IO [Int]
availableIPsQuery =
  do
    runQuery $(compileQuery $ unlines
             [ "select oct from deployapp.octal"
             , "where oct between 3 and 99"
             , "and not exists "
             , "("
             , "  select ip_address"
             , "  from deployapp.host"
             , "  where ip_address = oct"
             , ")"
             ])
-}

hostsQuery :: IO [Host]
hostsQuery = 
  do    
    map hostFromTuple <$> runQuery $(compileQuery $ unlines
              [ "select"
              , "  h.id"
              , ", h.hw_address"
              , ", h.profile_id"
              , ", last_reported_ip"
              , ", d.ip_assignment"
              , "from host h"
              , "left outer join dhcp_entry d"
              , "on h.id = d.host_id;"
              ])
    

updateHostProfileQuery :: Int -> Int -> IO Integer
updateHostProfileQuery host profile =
  do
    runQuery $(compileQuery $ unlines
              [ "update host"
              , "set profile_id = ?profile"
              , "where id = ?host;"
              ])

updateHostIPQuery :: Int -> Int -> IO Integer
updateHostIPQuery host ip =
  do
    runQuery $(compileQuery $ unlines
              [ "insert into dhcp_entry"
              , "(host_id, ip_assignment)"
              , "values (?host, ?ip);"
              ])

unassignHostProfileQuery :: Int -> IO Integer
unassignHostProfileQuery host =
  do
    runQuery $(compileQuery $ unlines
              [ "update host"
              , "set profile_id = null"
              , "where id = ?host;"
              ])

unassignHostIPQuery :: Int -> IO Integer
unassignHostIPQuery host =
  do
    runQuery $(compileQuery $ unlines
              [ "delete dhcp_entry"
              , "where host_id = ?host;"
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
              , ", max(?expartn,"
              , "      (select dp.partition_number"    
              , "      from disk_partition dp"
              , "      where dp.disk_id = ?disk"
              , "      and dp.partition_type = 0))"
              , ", ?expartt"
              , ");" 
              ])
    

deletePartitionQuery :: Int -> IO Integer
deletePartitionQuery partitionid =
  do
    runQuery $(compileQuery $ unlines
              [ "delete from disk_partition"
              , "where id = ?partitionid"
              ])

dropMaybe4 :: [(Maybe a, Maybe b, Maybe c, Maybe d)] -> [(a,b,c,d)]
dropMaybe4 [] = []
dropMaybe4 ((Just a, Just b, Just c, Just d):t) 
           = (a,b,c,d) : dropMaybe4 t

fdiskQuery :: Int -> IO [Partition]
fdiskQuery host =
  do
    results <- runQuery $(compileQuery $ unlines
        [ "select"
        , "p.id, p.partition_number, p.partition_type, p.size_in_mb"
        , "from host h"
        , "left outer join profile f on h.profile_id = f.id"
        , "left outer join disk d on f.disk_id = d.id"
        , "left outer join disk_partition p on d.id = p.disk_id"
        , "where h.id = ?host;"
--        , "order by p.partition_number"
        ])
    let goodResults = dropMaybe4 results
    return . sort $ map partitionFromTuple goodResults

