
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

restartCommand :: CreateProcess
restartCommand = shell "systemctl restart dhcpd.service"

restartDhcpd :: IO ExitCode
restartDhcpd =
  do
    (_stdin, _stdout, _stderr, pid) <- createProcess restartCommand
    waitForProcess pid

rewriteDhcpdConfig :: IO ()
rewriteDhcpdConfig =
  do
    base <- B.readFile dhcpdBaseConfig
    hosts <- hostsQuery
    let output = B.concat (base : map (formatEntry.entryFromHost) hosts)
    B.writeFile dhcpdConfig output

data Entry = Entry { hw_address :: String, ip_address :: String }

formatEntry :: Entry -> B.ByteString
formatEntry (Entry hw_address' ip_address') =
  B.pack . unlines $ 
         [ "host {"
         , concat ["  hardware ethernet ", hw_address', ";"]
         , concat ["  fixed-address ", ip_address', ";"]
         , "}" 
         , "" 
         ]

entryFromHost :: Host -> Entry
entryFromHost (Host _id hw_address' {- ip_address -} _profid _profname) =
  Entry hw_address' "fak.efa.kef.ake"