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

runTestWithData :: [TestPair] -> IO String -- IO Performance info
runTestWithData vals = do
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
queryHashMap hmap = return $ map (\k -> ts_someCode (fromJust $ H.lookup k hmap)) (H.keys hmap)

runTest :: Int -> IO ()
runTest size = do
    testData <- genData size
    perfInfo <- runTestWithData testData
    putStrLn $ perfInfo

main = do
    testData <- genData 100000
    perfInfo <- runTestWithData testData
    putStrLn perfInfo
