module MainTest where

import Control.Monad (join)
import Data.List (intersperse)

import qualified RedisTest as RT
import TestData

main = do
    dataSize <- parseArgs
    testData <- genData dataSize
    redisOutput <- testRedis testData
    hashtablesOutput <- testHashtables testData


testHashtables :: [TestStruct] -> IO String
testHashtables = undefined
