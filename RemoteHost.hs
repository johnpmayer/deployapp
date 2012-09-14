
{-# OPTIONS -Wall #-}

module RemoteHost where

--import System.Exit
import System.Process

import Utils

pingCommand :: Int -> CreateProcess
pingCommand ip = shell $ "ping -c 4 192.168.1." ++ show ip

pingHost :: Int -> IO Int
pingHost ip = 
  do
    pingCode <- simpleRunProcess $ pingCommand ip
    return $ getExitCode pingCode

