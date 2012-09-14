
{-# OPTIONS -Wall #-}
--{-# LANGUAGE #-}

module DhcpdConfig where

import qualified Data.ByteString.Lazy.Char8 as B

import System.Exit
import System.Process

import Queries 
import Types

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
    (_stdin, _stdout, _stderr, pid1) <- createProcess restartCommand
    _ <- waitForProcess pid1
    (_stdin, _stdout, _stderr, pid2) <- createProcess statusCommand
    _ <- waitForProcess pid2
    return ()

rewriteDhcpdConfig :: IO ()
rewriteDhcpdConfig =
  do
    base <- B.readFile dhcpdBaseConfig
    hosts <- hostsQuery
    let leased_hosts = filter (\h -> ip_address h /= Nothing) hosts
    let entries = map entryFromHost leased_hosts
    let output = B.concat (base : map formatEntry entries)
    B.writeFile dhcpdConfig output

data Entry = Entry { hw_address :: String
                   , leased_ip_address :: Maybe Int 
                   }

formatEntry :: Entry -> B.ByteString
formatEntry (Entry hw_address' ip_address') =
  B.pack $ case ip_address' of
    Nothing -> ""
    Just lease -> unlines $ 
         [ concat ["host ", filter (/=':') hw_address', " {"]
         , concat ["  hardware ethernet ", hw_address', ";"]
         , concat ["  fixed-address 192.168.1.", show lease, ";"]
         , "}"
         , "" 
         ]

entryFromHost :: Host -> Entry
entryFromHost (Host _id hw_address' ip_address' _profid) =
  Entry hw_address' ip_address'