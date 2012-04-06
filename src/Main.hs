module Main where

import Diagrams.OSM
import Diagrams.Prelude
import Diagrams.Backend.Postscript
import Diagrams.Backend.Postscript.CmdLine

import Data.List

import System.Environment
 
findOsm as = go [] as
  where 
    go rs (a:as)
      | ".osm" `isSuffixOf` a = Just (a, foldl' (flip (:)) as rs)
      | otherwise             = go (a:rs) as
    go _ []                   = Nothing

main = do
  as <- getArgs
  case findOsm as of
    Nothing -> putStrLn "Expected OSM file as argument."
    Just (f,as') -> do
      ed <- buildOsm defaultOsmStyle f
      case ed of
        Left e  -> putStrLn e
        Right d -> withArgs as' (defaultMain d)
