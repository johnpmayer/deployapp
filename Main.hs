
{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Main where

import           Control.Applicative
import           Control.Monad.IO.Class

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.List

--import           Data.Tuple.Sequence

import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

import           DhcpdConfig
import           Queries
import           RemoteHost
import           Types
import           Utils

main :: IO ()
main =
  do
    reconfigureDhcpd
    quickHttpServe site

site :: Snap ()
site =
     
     ifTop (serveFile "index.html") <|>
     dir "static" (serveDirectory "static") <|>
     dir "images" (serveDirectory "images") <|>
     dir "repo" (serveDirectory "repo") <|>

     route [ ("app/hosts", method GET $ makeJSONHandler hostsQuery)
           , ("app/host/profile", method POST updateHostProfile <|>
                                  method DELETE unassignHostProfile)
           , ("app/host/ip", method POST   updateHostIP <|>
                             method DELETE unassignHostIP)
           , ("app/host/ping", method POST pingHostIP)
           , ("app/host/reboot", method POST $ rebootHost)
           
           , ("app/profiles", method GET $ makeJSONHandler profilesQuery)
           , ("app/profile", method PUT createProfile <|>
                             method DELETE deleteProfile)

           , ("app/profile/:profile_id/packages", method GET getProfilePackages)
           , ("app/profile/package", method PUT addPackageToProfile <|>
                                     method DELETE removePackageFromProfile)

           , ("app/images", method GET $ makeJSONHandler imagesQuery)

           , ("app/softwares", method GET $ makeJSONHandler softwaresQuery)
                             
           , ("app/disks", method GET $ makeJSONHandler disksQuery)
           , ("app/disk", method PUT createDisk <|>
                          method DELETE deleteDisk)
                          
           , ("app/disk/:disk_id/partitions", method GET getPartitions)
           , ("app/disk/partition", method PUT createPartition <|>
                               method DELETE deletePartition)

           , ("core/check/:mac", method GET checkHost)
           , ("core/register/:mac", method GET registerHost)
           , ("core/report/:host_id/:ip", method GET reportHostIP)
           , ("core/fdisk/:host_id", method GET fdisk)
           , ("core/fstab/:host_id", method GET fstab)
           , ("core/archive/:host_id", method GET archive)
           ]

{- APP -}

updateHostProfile :: Snap ()
updateHostProfile = 
  do
    host <- requireInt "host_id"
    profile <- requireInt "profile_id"
    makeJSONHandler $ updateHostProfileQuery host profile

updateHostIP :: Snap ()
updateHostIP =
  do
    host <- requireInt "host_id"
    ip <- requireInt "ip_address"
    makeJSONHandler $ updateHostIPQuery host ip
    liftIO reconfigureDhcpd

unassignHostProfile :: Snap ()
unassignHostProfile =
  do
    host <- requireInt "host_id"
    makeJSONHandler $ unassignHostProfileQuery host

unassignHostIP :: Snap ()
unassignHostIP =
  do
    host <- requireInt "host_id"
    makeJSONHandler $ unassignHostIPQuery host
    liftIO reconfigureDhcpd

pingHostIP :: Snap ()
pingHostIP =
  do
    ip <- requireInt "ip_address"
    makeJSONHandler $ pingHost ip

rebootHost :: Snap ()
rebootHost =
  do
    ip <- requireInt "ip_address"
    makeJSONHandler $ runRebootHost ip

createProfile :: Snap ()
createProfile =
  do
    name <- requireString "name"
    desc <- requireString "description"
    disk <- requireInt "disk_id"
    image <- requireInt "image_id"
    makeJSONHandler $ newProfileQuery name desc disk image

deleteProfile :: Snap ()
deleteProfile =
  do
    profile_id' <- requireInt "profile_id"
    makeJSONHandler $ deleteProfileQuery profile_id'

getProfilePackages :: Snap ()
getProfilePackages =
  do               
    profile_id' <- requireInt "profile_id"
    makeJSONHandler $ getProfilePackagesQuery profile_id'

addPackageToProfile :: Snap ()
addPackageToProfile =
  do
    profile_id' <- requireInt "profile_id"
    package_id' <- requireInt "package_id"
    makeJSONHandler $ addPackageToProfileQuery profile_id' package_id'

removePackageFromProfile :: Snap ()
removePackageFromProfile =
  do
    profile_id' <- requireInt "profile_id"
    package_id' <- requireInt "package_id"
    makeJSONHandler $ removePackageFromProfileQuery profile_id' package_id'



createDisk :: Snap ()
createDisk =
  do
    disk_name' <- requireString "disk_name"
    makeJSONHandler $ newDiskQuery disk_name'

deleteDisk :: Snap ()
deleteDisk =
  do
    disk_id' <- requireInt "disk_id"
    makeJSONHandler $ deleteDiskQuery disk_id'

getPartitions :: Snap ()
getPartitions = 
  do
    disk_id' <- requireInt "disk_id"
    makeJSONHandler $ diskPartitionsQuery disk_id'

createPartition :: Snap ()
createPartition =
  do
    disk <- requireInt "disk_id"
    number <- requireInt "partition_number"
    partition_type' <- requireString "partition_type"
    mount <- requireString "mount_point"
    boot <- requireInt "is_boot"
    size <- requireInt "size_in_mb"
    case partition_type' of
      "Primary"  -> makeJSONHandler $ 
                    newPrimaryPartitionQuery  disk number mount boot size
      "Extended" -> makeJSONHandler $ 
                    newExtendedPartitionQuery disk number mount boot size
      "Logical"  -> makeJSONHandler $ 
                    newLogicalPartitionQuery  disk number mount boot size
      _          -> pass

deletePartition :: Snap ()
deletePartition =
  do
    partition' <- requireInt "partition_id"
    makeJSONHandler $ deletePartitionQuery partition'

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

reportHostIP :: Snap ()
reportHostIP =
  do
    host_id' <- requireInt "host_id"
    host_ip' <- requireInt "ip"
    makeJSONHandler $ reportHostIPQuery host_id' host_ip'

fdisk :: Snap ()
fdisk = 
  do
    host_id' <- requireInt "host_id"
    partitions <- liftIO $ fdiskQuery host_id'
    writeLBS . B.pack . unlines . map fdiskEntry $ partitions

compareBy :: Ord b => (a -> b) -> a -> a -> Ordering
compareBy f x y = compare (f x) (f y)

fstab :: Snap ()
fstab = 
  do
    host_id' <- requireInt "host_id"
    partitions <- liftIO $ fdiskQuery host_id'
    let mountOrderPartitions = sortBy (compareBy mount_point) partitions
    writeLBS . B.pack . unlines . map fdiskEntry $ mountOrderPartitions

archive :: Snap ()
archive =
  do
    host_id' <- requireInt "host_id"
    archive_url' <- requireOne $ archiveQuery host_id'
    writeLBS . B.pack $ archive_url'