
{-# OPTIONS -Wall #-}
--{-# LANGUAGE #-}

module DhcpdConfig where

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char
import           Data.Maybe

import System.Process

import Queries 
import Types
import Utils

dhcpDir :: FilePath
dhcpDir = "/etc/dhcp/"

dhcpdBaseConfig :: FilePath
dhcpdBaseConfig = dhcpDir ++ "dhcpd.conf.base"

dhcpdConfig :: FilePath
dhcpdConfig = dhcpDir ++ "dhcpd.conf"

reconfigureDhcpd :: IO ()
reconfigureDhcpd = 
  do
    rewriteDhcpdConfig
    restartDhcpd

restartCommand :: CreateProcess
restartCommand = shell "systemctl restart dhcpd.service"

statusCommand :: CreateProcess
statusCommand = shell "systemctl status dhcpd.service; cat /etc/dhcp/dhcpd.conf"

restartDhcpd :: IO ()
restartDhcpd =
  do
    _restartCode <- simpleRunProcess restartCommand
    _statusCode <- simpleRunProcess statusCommand
    return ()

rewriteDhcpdConfig :: IO ()
rewriteDhcpdConfig =
  do
    base <- B.readFile dhcpdBaseConfig
    hosts <- hostsQuery
    let entries = catMaybes $ map entryFromHost hosts
    let output = B.concat (base : map formatEntry entries)
    B.writeFile dhcpdConfig output

data Entry = Entry { hw_ethernet :: String
                   , fixed_address :: Int
                   }

formatEntry :: Entry -> B.ByteString
formatEntry (Entry hw_ethernet' fixed_address') =
  B.pack . unlines $ 
         [ concat ["host ", filter (/=':') hw_ethernet', " {"]
         , concat ["  hardware ethernet ", hw_ethernet', ";"]
         , concat ["  fixed-address 192.168.1.", show fixed_address', ";"]
         , "}"
         , "" 
         ]

entryFromHost :: Host -> Maybe Entry
entryFromHost host = do
  fixed_address' <- ip_assignment host -- auto failthrough
  let hw_ethernet' = hw_address host
  return $ Entry hw_ethernet' fixed_address'


symlinkLocalboot :: String -> CreateProcess
symlinkLocalboot hostMAC = 
  shell $ "ln -sf localboot "
        ++ "/var/lib/tftpboot/pxelinux.cfg/01-" ++ mac_dash_lower
  where mac_dash_lower = map toLower 
                       . map (\c -> if c == ':' then '-' else c)
                       $ hostMAC

switchLocalbootPXE :: String -> IO ()
switchLocalbootPXE hostMAC = 
  do 
    _linkCode <- simpleRunProcess $ symlinkLocalboot hostMAC
    return ()
