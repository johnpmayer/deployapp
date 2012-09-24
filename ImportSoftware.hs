
{-# Options -Wall #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module ImportSoftware where

import Utils

addSoftware :: String -> IO Integer
addSoftware package =
  do
    runQuery $(compileQuery $ unlines
             [ "insert into software"
             , "(package_name)"
             , "values (?package)"
             ])

getPackages :: IO [String]
getPackages = fmap lines (readFile "repos.txt")

run :: IO ()
run = do packages <- getPackages
         mapM_ addSoftware packages