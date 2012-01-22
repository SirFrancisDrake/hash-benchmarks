module HashtablesTest 
  ( runTest
  )
  where

import Control.Applicative ((<$>))
import Control.DeepSeq
import Control.Exception
import Data.Maybe (fromJust)
import Data.Time.Clock
import qualified Data.HashMap as H

import TestData

runTest :: [TestPair] -> IO String -- IO Performance info
runTest vals = do
    time <- getCurrentTime
    hmap <- genHashMap vals
    evaluate hmap
    setTime <- (flip diffUTCTime time) <$> getCurrentTime
    time <- getCurrentTime
    hresult <- queryHashMap hmap
    evaluate (rnf hresult)
    getTime <- (flip diffUTCTime time) <$> getCurrentTime
    return $ "Set time: " ++ show setTime ++ "\nGet time: " ++ show getTime


genHashMap :: [TestPair] -> IO (H.HashMap String TestStruct)
genHashMap vals = return $ H.fromList vals

queryHashMap :: (H.HashMap String TestStruct) -> IO [String]
queryHashMap hmap = return $ map (\k -> ts_someCode (fromJust $ H.lookup k hmap) ++ "hi") (H.keys hmap)

main = do
    testData <- genData 100000
    perfInfo <- runTest testData
    putStrLn perfInfo
