
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
import           UserSession
import           Utils

main :: IO ()
main =
  do
    reconfigureDhcpd
    sessionStore <- emptySessionStore
    quickHttpServe (site sessionStore)

site :: SessionStore -> Snap ()
site store =
          
     ifTop (serveFile "index.html") <|>
     dir "static" (serveDirectory "static") <|>
     dir "images" (serveDirectory "images") <|>
     dir "repo" (serveDirectory "repo") <|>

     route [ ("app/login", method POST $ loginHandler store)
           , ("app/check", method GET $ checkHandler store)
           , ("app/hosts", method GET $ hostsHandler store)
           , ("app/host/profile", method POST (updateHostProfile store) <|>
                                  method DELETE (unassignHostProfile store))
           , ("app/host/ip", method POST   (updateHostIP store) <|>
                             method DELETE (unassignHostIP store))
           , ("app/host/stage", method POST (stageHost store))
           , ("app/host/ping", method POST (pingHostIP store))
           , ("app/host/reboot", method POST $ (rebootHost store))
           
           , ("app/profiles", method GET $ (profilesHandler store))
           , ("app/profile", method PUT (createProfile store) <|>
                             method DELETE (deleteProfile store))

           , ("app/profile/:profile_id/packages", 
                method GET (getProfilePackages store))

           , ("app/profile/package", method PUT (addPackageToProfile store) <|>
                                     method DELETE (removePackageFromProfile store))

           , ("app/images", method GET $ imagesHandler store)

           , ("app/softwares", method GET $ softwaresHandler store)
                             
           , ("app/disks", method GET $ disksHandler store)
           , ("app/disk", method PUT (createDisk store) <|>
                          method DELETE (deleteDisk store))
                          
           , ("app/disk/:disk_id/partitions", method GET (getPartitions store))
           , ("app/disk/partition", method PUT (createPartition store) <|>
                               method DELETE (deletePartition store))
           ] <|>

     route [ ("core/check/:mac", method GET checkHost)
           , ("core/register/:mac", method GET registerHost)
           , ("core/report/:host_id/:ip", method GET reportHostIP)
           , ("core/isstaged/:host_id", method GET isHostStaged)
           , ("core/fdisk/:host_id", method GET fdisk)
           , ("core/fstab/:host_id", method GET fstab)
           , ("core/archive/:host_id", method GET archive)
           , ("core/packages/:host_id", method GET packages)
           , ("core/finished/:host_id", method GET finished)
           ]

{- APP -}

-- Basic gets

hostsHandler :: SessionStore -> Snap ()
hostsHandler store =
  do
    clearSecurityLevel store ["get hosts"] ReadOnly
    makeJSONHandler hostsQuery

profilesHandler :: SessionStore -> Snap ()
profilesHandler store =
  do
    clearSecurityLevel store ["get profiles"] ReadOnly
    makeJSONHandler profilesQuery

imagesHandler :: SessionStore -> Snap ()
imagesHandler store =
  do
    clearSecurityLevel store ["get images"] ReadOnly
    makeJSONHandler imagesQuery

softwaresHandler :: SessionStore -> Snap ()
softwaresHandler store =
  do
    clearSecurityLevel store ["get softwares"] ReadOnly
    makeJSONHandler softwaresQuery

disksHandler :: SessionStore -> Snap ()
disksHandler store =
  do
    clearSecurityLevel store ["get disks"] ReadOnly
    makeJSONHandler disksQuery

-- takes some params

updateHostProfile :: SessionStore -> Snap ()
updateHostProfile store = 
  do
    host <- requireInt "host_id"
    profile <- requireInt "profile_id"
    clearSecurityLevel store ["update host profile", show host, show profile] 
                       ReadWrite
    makeJSONHandler $ updateHostProfileQuery host profile

updateHostIP :: SessionStore -> Snap ()
updateHostIP store =
  do
    host <- requireInt "host_id"
    ip <- requireInt "ip_address"
    clearSecurityLevel store ["assign host ip", show host, show ip] ReadWrite
    makeJSONHandler $ updateHostIPQuery host ip
    liftIO reconfigureDhcpd

unassignHostProfile :: SessionStore -> Snap ()
unassignHostProfile store =
  do
    host <- requireInt "host_id"
    clearSecurityLevel store ["unassign host profile", show host] ReadWrite
    makeJSONHandler $ unassignHostProfileQuery host

unassignHostIP :: SessionStore -> Snap ()
unassignHostIP store =
  do
    host <- requireInt "host_id"
    clearSecurityLevel store ["unassign host ip", show host] ReadWrite
    makeJSONHandler $ unassignHostIPQuery host
    liftIO reconfigureDhcpd

stageHost :: SessionStore -> Snap ()
stageHost store =
  do
    host <- requireInt "host_id"
    clearSecurityLevel store ["stage host", show host] Admin
    makeJSONHandler $ stageHostQuery host

pingHostIP :: SessionStore -> Snap ()
pingHostIP store =
  do
    ip <- requireInt "ip_address"
    clearSecurityLevel store ["ping host", show ip] ReadOnly
    makeJSONHandler $ pingHost ip

rebootHost :: SessionStore -> Snap ()
rebootHost store =
  do
    ip <- requireInt "ip_address"
    clearSecurityLevel store ["reboot host", show ip] Admin
    makeJSONHandler $ runRebootHost ip

createProfile :: SessionStore -> Snap ()
createProfile store =
  do
    name <- requireString "name"
    desc <- requireString "description"
    disk <- requireInt "disk_id"
    image <- requireInt "image_id"
    clearSecurityLevel store ["create profile", name, desc, show disk, show image] 
                       ReadWrite
    makeJSONHandler $ newProfileQuery name desc disk image

deleteProfile :: SessionStore -> Snap ()
deleteProfile store =
  do
    profile_id' <- requireInt "profile_id"
    clearSecurityLevel store ["delete profile", show profile_id'] ReadWrite
    makeJSONHandler $ deleteProfileQuery profile_id'

getProfilePackages :: SessionStore -> Snap ()
getProfilePackages store =
  do               
    profile_id' <- requireInt "profile_id"
    clearSecurityLevel store ["get profile packages", show profile_id'] ReadOnly
    makeJSONHandler $ getProfilePackagesQuery profile_id'

addPackageToProfile :: SessionStore -> Snap ()
addPackageToProfile store =
  do
    profile_id' <- requireInt "profile_id"
    package_id' <- requireInt "package_id"
    clearSecurityLevel store ["add package to profile"
                             , show profile_id'
                             , show package_id']
                       ReadWrite
    makeJSONHandler $ addPackageToProfileQuery profile_id' package_id'

removePackageFromProfile :: SessionStore -> Snap ()
removePackageFromProfile store =
  do
    profile_id' <- requireInt "profile_id"
    package_id' <- requireInt "package_id"
    clearSecurityLevel store ["remote package from profile"
                             , show profile_id'
                             , show package_id']
                             ReadWrite
    makeJSONHandler $ removePackageFromProfileQuery profile_id' package_id'

createDisk :: SessionStore -> Snap ()
createDisk store =
  do
    disk_name' <- requireString "disk_name"
    clearSecurityLevel store ["create disk", disk_name'] ReadWrite
    makeJSONHandler $ newDiskQuery disk_name'

deleteDisk :: SessionStore -> Snap ()
deleteDisk store =
  do
    disk_id' <- requireInt "disk_id"
    clearSecurityLevel store ["delete disk", show disk_id'] ReadWrite
    makeJSONHandler $ deleteDiskQuery disk_id'

getPartitions :: SessionStore -> Snap ()
getPartitions store = 
  do
    disk_id' <- requireInt "disk_id"
    clearSecurityLevel store ["get disk partitions", show disk_id'] ReadOnly
    makeJSONHandler $ diskPartitionsQuery disk_id'

createPartition :: SessionStore -> Snap ()
createPartition store =
  do
    disk <- requireInt "disk_id"
    number <- requireInt "partition_number"
    partition_type' <- requireString "partition_type"
    mount <- requireString "mount_point"
    boot <- requireInt "is_boot"
    size <- requireInt "size_in_mb"
    clearSecurityLevel store ["create partition", show disk, show number
                             ,partition_type', mount, show boot, show size]
                             ReadWrite
    case partition_type' of
      "Primary"  -> makeJSONHandler $ 
                    newPrimaryPartitionQuery  disk number mount boot size
      "Extended" -> makeJSONHandler $ 
                    newExtendedPartitionQuery disk number mount boot size
      "Logical"  -> makeJSONHandler $ 
                    newLogicalPartitionQuery  disk number mount boot size
      _          -> pass

deletePartition :: SessionStore -> Snap ()
deletePartition store =
  do
    partition' <- requireInt "partition_id"
    clearSecurityLevel store ["delete partition", show partition'] ReadWrite
    makeJSONHandler $ deletePartitionQuery partition'

{- CORE -}

checkHost ::  Snap ()
checkHost =
  do
    mac <- requireString "mac"
    makeJSONHandler $ checkHostQuery mac

registerHost ::  Snap ()
registerHost =
  do
    mac <- requireString "mac"
    makeJSONHandler $ registerHostQuery mac

reportHostIP ::  Snap ()
reportHostIP =
  do
    host_id' <- requireInt "host_id"
    host_ip' <- requireInt "ip"
    makeJSONHandler $ reportHostIPQuery host_id' host_ip'

isHostStaged ::  Snap ()
isHostStaged = 
  do 
    host <- requireInt "host_id"
    deploy_stage' <- requireOne $ hostIsStagedQuery host
    writeLBS . B.pack . show $ deploy_stage'

fdisk ::  Snap ()
fdisk = 
  do
    host_id' <- requireInt "host_id"
    partitions <- liftIO $ fdiskQuery host_id'
    writeLBS . B.pack . unlines . map fdiskEntry $ partitions

compareBy :: Ord b => (a -> b) -> a -> a -> Ordering
compareBy f x y = compare (f x) (f y)

fstab ::  Snap ()
fstab = 
  do
    host_id' <- requireInt "host_id"
    partitions <- liftIO $ fdiskQuery host_id'
    let mountOrderPartitions = sortBy (compareBy mount_point) partitions
    writeLBS . B.pack . unlines . map fdiskEntry $ mountOrderPartitions

archive ::  Snap ()
archive =
  do
    host_id' <- requireInt "host_id"
    archive_url' <- requireOne $ archiveQuery host_id'
    writeLBS . B.pack $ archive_url'

packages ::  Snap ()
packages = do host_id' <- requireInt "host_id"
              packages' <- liftIO $ getHostPackagesQuery host_id'
              writeLBS . B.pack . unlines $ packages'

finished ::  Snap ()
finished = do host <- requireInt "host_id"
              mac <- requireOne $ getHostMacQuery host
              liftIO $ switchLocalbootPXE mac
              makeJSONHandler $ markHostHotQuery host