module TestData where

import qualified Data.ByteString as B

import DataGenerator

type TestPair = (B.ByteString,B.ByteString)

genData :: Params -> IO [TestPair]
genData = sample 
