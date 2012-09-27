
{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UserSession where

import Control.Applicative
import Control.Monad.IO.Class

import Data.ByteString.Char8 (ByteString, pack)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time.Clock

import Snap.Core

import System.Random

import Utils

data SecurityLevel = None | ReadOnly | ReadWrite | Admin deriving (Eq, Ord, Show)

data User = User { username :: String
                 , password :: String
                 , security :: SecurityLevel
                 } deriving (Show)

admin :: User
admin = User "admin" "awesome" ReadWrite

report :: User
report = User "report" "easy" ReadOnly

type UserStore = Map String User

userStore :: UserStore
userStore = M.fromList $ zip (map username users) users
              where users = [admin, report]

data Session = Session { user :: User
                       , expiry :: UTCTime
                       } deriving (Show)

sessionCookieName :: ByteString
sessionCookieName = pack "session"

type SessionStore = IORef (Map ByteString Session)

emptySessionStore :: IO (SessionStore)
emptySessionStore = newIORef M.empty

changelog :: User -> [String] -> IO ()
changelog currentUser action = 
  putStrLn $ "Granted user: " ++ show currentUser ++ " action: " ++ show action
            --ToDo shove this into a log file rather than stdout

clearSecurityLevel :: SessionStore -> [String] -> SecurityLevel -> Snap ()
clearSecurityLevel storeRef action requiredLevel =
  do
    currentUser <- requireSessionUser storeRef
    let sessionLevel = security currentUser
    if sessionLevel < requiredLevel
    then noAccessHandler >> withResponse finishWith
    else liftIO $ changelog currentUser action
{-    
    liftIO $ putStrLn.show $ ("wants clearance" : action)
    current <- liftIO getCurrentTime
    sessionId <- readCookie $ sessionCookieName
    liftIO $ putStrLn.show $ "session:"++show sessionId
    session <- liftIO $ M.lookup sessionId <$> readIORef storeRef
    case session of
      Nothing -> noAccessHandler
      Just s -> if expiry s > current && (security . user $ s) >= level
                then do liftIO $ changelog (show action)
                        return ()
                else noAccessHandler
-}

getSession :: SessionStore -> Snap (Maybe Session)
getSession storeRef = 
  do    
    sessionId <- readCookie $ sessionCookieName
    liftIO $ M.lookup sessionId <$> readIORef storeRef

requireSessionUser :: SessionStore -> Snap User
requireSessionUser storeRef =
  do current <- liftIO getCurrentTime
     mSession <- getSession storeRef
     case mSession of
                Nothing -> noAccessHandler
                (Just s) -> 
                   if expiry s > current 
                   then return $ user s 
                   else noAccessHandler

getSessionSecurity :: SessionStore -> Snap SecurityLevel
getSessionSecurity storeRef =
  do current <- liftIO getCurrentTime
     mSession <- getSession storeRef
     return $ case mSession of
                Nothing -> None
                (Just s) -> 
                   if expiry s > current 
                   then security . user $ s 
                   else None

createSession :: SessionStore -> Session -> IO ByteString
createSession storeRef session = 
  do sessionId <- pack . show <$> (randomIO :: IO Int)
     atomicModifyIORef storeRef (\store -> (M.insert sessionId session store, ()))
     readIORef storeRef >>= putStrLn.show
     return $ sessionId

tryUserLogin :: SessionStore -> Maybe User -> String -> Snap ()
tryUserLogin _ Nothing _ = makeJSONHandler $ return False
tryUserLogin sessionStore (Just u) pwd
  | pwd /= password u = makeJSONHandler $ return False
  | otherwise = do life <- liftIO $ (addUTCTime 300) <$> getCurrentTime
                   let session = Session u life
                   sessionId <- liftIO $ createSession sessionStore session
                   let cookie = Cookie { cookieName     = sessionCookieName
                                       , cookieValue    = sessionId
                                       , cookieExpires  = Nothing
                                       , cookieDomain   = Nothing
                                       , cookiePath     = Nothing
                                       , cookieSecure   = False
                                       , cookieHttpOnly = True
                                       }
                   modifyResponse $ addResponseCookie cookie
                   makeJSONHandler $ return True

checkHandler :: SessionStore -> Snap ()
checkHandler sessionStore =
  do currentLevel <- getSessionSecurity sessionStore
     makeJSONHandler $ return (currentLevel > None)

loginHandler :: SessionStore -> Snap ()
loginHandler sessionStore = 
  do name <- requireString "username"
     pwd <- requireString "password"
     tryUserLogin sessionStore (M.lookup name userStore) pwd

