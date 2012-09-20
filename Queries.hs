
{-# Options -Wall #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Queries where

import Control.Applicative

import Data.List
import Data.Maybe
import Data.Tuple.Curry

{-
import Database.HDBC
import Database.HDBC.ODBC
import Database.MetaHDBC
-}

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

hostsQuery :: IO [Host]
hostsQuery = 
  do    
    map (uncurryN Host) <$> runQuery $(compileQuery $ unlines
              [ "select"
              , "  h.id"
              , ", h.hw_address"
              , ", h.profile_id"
              , ", h.last_reported_ip"
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
    map (uncurryN Profile) <$> runQuery $(compileQuery $ unlines
              [ "select"
              , "  id"
              , ", name"
              , ", description"
              , ", disk_id"
              , ", image_id"
              , "from profile;"
              ])
    

newProfileQuery :: String -> String -> Int -> Int -> IO Integer
newProfileQuery name desc disk image =
  do
    runQuery $(compileQuery $ unlines
              [ "insert into profile"
              , "(name, description, disk_id, image_id)"
              , "values"
              , "(?name, ?desc, ?disk, ?image);"
              ])
    

deleteProfileQuery :: Int -> IO Integer
deleteProfileQuery profile =
  do
    runQuery $(compileQuery $
              "delete from profile where id = ?profile;")

imagesQuery :: IO [Image]
imagesQuery = 
  map (uncurryN Image) <$> runQuery $(compileQuery $
                             "select id, name, archive_url from image")

softwaresQuery :: IO [Software]
softwaresQuery = 
  map (uncurryN Software) <$> runQuery $(compileQuery $
                     "select id, package_name from software")

disksQuery :: IO [Disk]
disksQuery =
  do
    map (uncurryN Disk) <$> runQuery $(compileQuery $ unlines
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
    map (uncurryN Partition) <$> runQuery $(compileQuery $ unlines
              [ "select"
              , "  id"
              , ", partition_number"
              , ", partition_type"
              , ", mount_point"
              , ", is_boot"
              , ", size_in_mb"
              , "from disk_partition"
              , "where disk_id = ?disk;"
              ])
    

newPrimaryPartitionQuery :: Int -> Int -> String -> Int -> Int -> IO Integer
newPrimaryPartitionQuery disk number mount boot size =
  do
    runQuery $(compileQuery $ unlines
              [ "insert into disk_partition"
              , "( disk_id"
              , ", partition_number"
              , ", partition_type"
              , ", mount_point"
              , ", is_boot"
              , ", size_in_mb"
              , ", chain_partition_number"
              , ", chain_partition_type"
              , ") values (?disk, ?number, ?number, ?mount"
              , "         , ?boot, ?size, null, null);"
              ])
    
    
newExtendedPartitionQuery :: Int -> Int -> String -> Int -> Int -> IO Integer
newExtendedPartitionQuery disk number mount boot size =
  do
    runQuery $(compileQuery $ unlines
              [ "insert into disk_partition"
              , "( disk_id"
              , ", partition_number"
              , ", partition_type"
              , ", mount_point"
              , ", is_boot"
              , ", size_in_mb"
              , ", chain_partition_number"
              , ", chain_partition_type"
              , ") values (?disk, ?number, 0, ?mount, ?boot, ?size, null, null);"
              ])
    
    
newLogicalPartitionQuery :: Int -> Int -> String -> Int -> Int -> IO Integer
newLogicalPartitionQuery disk number mount boot size =
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
              , ", mount_point"
              , ", is_boot"
              , ", size_in_mb"
              , ", chain_partition_number"
              , ", chain_partition_type"
              , ")"
              , "values"
              , "( ?disk, ?number, ?number, ?mount, ?boot, ?size"
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

dropMaybe4 :: (Maybe a, Maybe b, Maybe c, Maybe d) -> Maybe (a,b,c,d)
dropMaybe4 (Just a, Just b, Just c, Just d) = Just (a,b,c,d)
dropMaybe4 _                                = Nothing

fdiskQuery :: Int -> IO [Partition]
fdiskQuery host =
  do
    results <- runQuery $(compileQuery $ unlines
        [ "select"
        , "  p.id"
        , ", p.partition_number"
        , ", p.partition_type"
        , ", p.mount_point"
        , ", p.is_boot"
        , ", p.size_in_mb"
        , "from disk_partition p"
        , "inner join disk    d on d.id         = p.disk_id"
        , "inner join profile f on f.disk_id    = d.id"
        , "inner join host    h on h.profile_id = f.id"
        , "where h.id = ?host;"
        ])
    return . sort . map (uncurryN Partition) $ results

archiveQuery :: Int -> IO [String]
archiveQuery host =
  do
    runQuery $(compileQuery $ unlines
            [ "select archive_url"
            , "from image i"
            , "inner join profile f on f.image_id   = i.id"
            , "inner join host    h on h.profile_id = f.id"
            , "where h.id = ?host;"
            ])
    