
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import Control.Applicative
import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B

import DataGenerator


-- TODO: PRNG seed as argument

main = do
  [name, v, n] <- getArgs
  let dataVariant = read v
  let numOfRecords = read n
  dat <- take numOfRecords <$> sample (params!!dataVariant) 
  case name of
    "generator" -> print $ length dat
    "data-map" -> testDataMap dat
  
  
testDataMap 
  = print
  . Map.member "dead beef"
  . Map.fromList
