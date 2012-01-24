import Control.Applicative ((<$>))
import Control.DeepSeq
import Control.Exception
import Data.Maybe (fromJust)
import Data.Time.Clock
import qualified Data.Map as H
import System (getArgs)

import TestData

runTestWithData :: [TestPair] -> IO String -- IO Performance info
runTestWithData vals = do
    time <- getCurrentTime
    let hmap = genHashMap vals
    evaluate hmap
    setTime <- (flip diffUTCTime time) <$> getCurrentTime
    time <- getCurrentTime
    let hresult = queryHashMap hmap
    evaluate (rnf hresult)
    getTime <- (flip diffUTCTime time) <$> getCurrentTime
    return $ "Set time: " ++ show setTime ++ "\nGet time: " ++ show getTime


genHashMap :: [TestPair] -> H.Map String TestStruct
genHashMap vals = H.fromList vals

queryHashMap :: H.Map String TestStruct -> [String]
queryHashMap hmap = map (\k -> ts_someCode (fromJust $ H.lookup k hmap)) (H.keys hmap)

runTest :: Int -> IO ()
runTest size = do
    testData <- genData size
    perfInfo <- runTestWithData testData
    putStrLn $ perfInfo

main = do
    args <- getArgs
    genData (read $ head args) >>= runTestWithData >>= putStrLn
