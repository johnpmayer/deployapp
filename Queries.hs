
{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Queries where

import Control.Applicative

import Database.HDBC
import Database.HDBC.ODBC
import Database.MetaHDBC

import Config
import Types
--import Utils

checkHostQuery :: String -> IO (Maybe Int)
checkHostQuery mac =
  do
    conn <- connectODBC deployDB
    let query = $(runStmt deployDB $ unlines
              [ "select id from host"
              , "where hw_address = ?mac;"
              ])
    query conn >>= (\results -> 
      case results of
        [host] -> return $ Just host
        _      -> return Nothing)

registerHostQuery :: String -> IO Integer
registerHostQuery mac =
  do
    conn <- connectODBC deployDB
    let query = $(runStmt deployDB $ unlines
              [ "insert into host"
              , "(hw_address, profile_id)"
              , "values (?mac, null);"
              ])
    withTransaction conn query

hostsQuery :: IO [Host]
hostsQuery = 
  do
    conn <- connectODBC deployDB
    let query = $(runStmt deployDB $ unlines
              [ "select"
              , "  h.id"
              , ", h.hw_address"
              , ", p.id"
              , ", p.name"
              , "from host h"
              , "left outer join profile p"
              , "on h.profile_id = p.id;"
              ])
    map hostFromTuple <$> query conn

profilesQuery :: IO [Profile]
profilesQuery = 
  do 
    conn <- connectODBC deployDB
    let query = $(runStmt deployDB $ unlines
              [ "select"
              , "  id"
              , ", name"
              , ", description"
              , ", disk_id"
              , "from profile;"
              ])
    map profileFromTuple <$> query conn

newProfileQuery :: String -> String -> Int -> IO Integer
newProfileQuery name desc disk =
  do
    conn <- connectODBC deployDB
    let query = $(runStmt deployDB $ unlines
              [ "insert into profile"
              , "(name, description, disk_id)"
              , "values"
              , "(?name, ?desc, ?disk);"
              ])
    withTransaction conn query

disksQuery :: IO [Disk]
disksQuery =
  do
    conn <- connectODBC deployDB
    let query = $(runStmt deployDB $ unlines
              [ "select"
              ,  "  id"
              ,  ", name"
              ,  "from disk;"
              ])
    map diskFromTuple <$> query conn

newDiskQuery :: String -> IO Integer
newDiskQuery diskname =
  do
    conn <- connectODBC deployDB
    let query = $(runStmt deployDB $ 
              "insert into disk (name) values (?diskname);")
    withTransaction conn query

deleteDiskQuery :: Int -> IO Integer
deleteDiskQuery disk =
  do
    conn <- connectODBC deployDB
    let query = $(runStmt deployDB $
              "delete from disk where id = ?disk;")
    withTransaction conn query
    
diskPartitionsQuery :: Int -> IO [Partition]
diskPartitionsQuery disk =
  do
    conn <- connectODBC deployDB
    let query = $(runStmt deployDB $ unlines
              [ "select"
              , "  id"
              , ", partition_number"
              , ", partition_type"
              , ", size_in_mb"
              , "from disk_partition"
              , "where disk_id = ?disk;"
              ])
    map partitionFromTuple <$> query conn

newPrimaryPartitionQuery :: Int -> Int -> Int -> IO Integer
newPrimaryPartitionQuery disk number size =
  do
    conn <- connectODBC deployDB
    let query = $(runStmt deployDB $ unlines
              [ "insert into disk_partition"
              , "( disk_id"
              , ", partition_number"
              , ", partition_type"
              , ", size_in_mb"
              , ", extended_partition_number"
              , ", extended_partition_type"
              , ") values (?disk, ?number, ?number"
              , "         , ?size, null, null);"
              ])
    withTransaction conn query
    
newExtendedPartitionQuery :: Int -> Int -> Int -> IO Integer
newExtendedPartitionQuery disk number size =
  do
    conn <- connectODBC deployDB
    let query = $(runStmt deployDB $ unlines
              [ "insert into disk_partition"
              , "( disk_id"
              , ", partition_number"
              , ", partition_type"
              , ", size_in_mb"
              , ", extended_partition_number"
              , ", extended_partition_type"
              , ") values (?disk, ?number, 0, ?size, null, null);"
              ])
    withTransaction conn query
    
newLogicalPartitionQuery :: Int -> Int -> Int -> IO Integer
newLogicalPartitionQuery disk number size =
  do
    let parttype = number
    conn <- connectODBC deployDB
    let query = $(runStmt deployDB $ unlines
              [ "insert into disk_partition"
              , "( disk_id"
              , ", partition_number"
              , ", partition_type"
              , ", size_in_mb"
              , ", extended_partition_number"
              , ", extended_partition_type"
              , ")"
              , "values"
              , "( ?disk, ?number, ?number, ?size"
              , ", (select dp.partition_number"    
              , "  from disk_partition dp"
              , "  where dp.disk_id = ?disk"
              , "  and dp.partition_type = 0)"
              , ", 0);" 
              ])
    withTransaction conn query

deletePartitionQuery :: Int -> IO Integer
deletePartitionQuery partitionid =
  do
    conn <- connectODBC deployDB
    let query = $(runStmt deployDB $ unlines
              [ "delete from disk_partition"
              , "where id = ?partitionid"
              ])
    withTransaction conn query

