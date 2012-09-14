
{-# OPTIONS -Wall #-}

module RemoteHost where

--import System.Exit
import System.Process

import Utils

pingCommand :: Int -> CreateProcess
pingCommand ip = shell $ "ping -c 4 192.168.1." ++ show ip

pingHost :: Int -> IO Int
pingHost = simpleRunProcess . pingCommand

{- remote commands -}

remoteCommand :: String -> Int -> CreateProcess
remoteCommand cmd ip = 
  shell $ "ssh 192.168.1." ++ show ip ++ " '" ++ cmd ++ "'"

deployLogCommand :: Int -> CreateProcess
deployLogCommand = remoteCommand "cat /tmp/deploylog"

getDeployLog :: Int -> IO Int
getDeployLog = simpleRunProcess . deployLogCommand

rebootCommand :: Int -> CreateProcess
rebootCommand = remoteCommand "sudo reboot"

runRebootHost :: Int -> IO Int
runRebootHost = simpleRunProcess . rebootCommand