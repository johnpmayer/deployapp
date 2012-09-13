
{-# OPTIONS -Wall #-}
{-# Language NoMonomorphismRestriction #-}

module Utils where

import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Aeson
import qualified Data.ByteString.Char8 as B

import           Database.HDBC
import           Database.MetaHDBC

import           Language.Haskell.TH

import           Snap.Core

-- project modules

--import Config

deployDB :: String
deployDB = "DSN=DeployDB;Uid=deployapp;Pwd=4data"

{- Database utilities -}

compileQuery :: String -> ExpQ
compileQuery = runStmt deployDB

runQuery :: Show a => (Connection -> IO a) -> IO a
runQuery query =
  do
    conn <- connectODBC deployDB
    result <- withTransaction conn query
    putStrLn . show $ result
    disconnect conn
    return result

{- Snap utilities -}

makeJSONHandler :: ToJSON a => IO a -> Snap ()
makeJSONHandler query = liftIO query >>= (writeLBS . encode)

sendJSONError :: String -> Snap ()
sendJSONError = writeLBS . encode

paramsError :: Snap ()
paramsError = sendJSONError "Error: Missing or Ill-Typed Parameters"

debugParams :: Snap ()
debugParams =
  do
    getParams >>= liftIO . putStrLn . show
    paramsError

requireParam :: (Maybe String -> Maybe a) -> 
                B.ByteString -> Snap a
requireParam convert key =
  do
    maybe_value_bs <- getParam key
    case convert $ fmap B.unpack maybe_value_bs of
      (Just val) -> return val
      Nothing -> debugParams >> pass

requireString :: B.ByteString -> Snap String
requireString = requireParam (fmap id)

requireInt :: B.ByteString -> Snap Int
requireInt = requireParam safeRead

maybeIntParam :: B.ByteString -> Snap (Maybe Int)
maybeIntParam key = 
  getParam key >>= return . safeRead . fmap B.unpack

safeRead :: Read a => Maybe String -> Maybe a
safeRead maybe_s = 
  do 
    s <- maybe_s
    case reads s of
      [(x, "")] -> return x
      _ -> mzero

