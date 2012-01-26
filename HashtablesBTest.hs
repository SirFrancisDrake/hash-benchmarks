import Control.Applicative ((<$>))
import Control.DeepSeq
import Control.Exception
import Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashTable.IO as H
import Data.Maybe (fromJust)
import Data.Time.Clock
import System (getArgs)
import System.Console.GetOpt
import System.Exit (exitFailure, exitSuccess)

import ByteStringInstance
import DataGenerator
import TestData
import Options
import ZagZag

type HashTable k v = H.BasicHashTable k v

runTestWithData :: [TestPair] -> IO String -- IO Performance info
runTestWithData vals = do
    time <- getCurrentTime
    hmap <- genHashtable vals
    evaluate hmap
    setTime <- (flip diffUTCTime time) <$> getCurrentTime
    time <- getCurrentTime
    hresult <- queryHashtable hmap
    evaluate (rnf hresult)
    getTime <- (flip diffUTCTime time) <$> getCurrentTime
    return $ "Basic Hashtable results\nSet time: " ++ show setTime ++ "\nGet time: " ++ show getTime

genHashtable :: [TestPair] -> IO (HashTable B.ByteString B.ByteString)
genHashtable vals = H.fromList vals

queryHashtable :: (HashTable B.ByteString B.ByteString) -> IO [(B.ByteString, B.ByteString)]
queryHashtable ht = H.toList ht

runTest :: (Params, [TestPair]) -> IO ()
runTest (pr, tps) = putStrLn ("\n" ++ (show pr) ++ "\nNumber of test pairs: " ++ show (length tps)) >> runTestWithData tps >>= putStrLn

spawnWorkingPairs :: [Params] -> [Int] -> IO [ (Params, [TestPair]) ]
spawnWorkingPairs params sizes = do
    let spawnWorkingPair (param, size) = take size <$> sample param >>= \ps -> return (param, ps)
    mapM spawnWorkingPair (makeTuples params sizes)

main = do
    args <- getArgs
    opts <- case getOpt RequireOrder options args of
        (o, [], []) -> return $ foldl (flip id) defOpts o
        (_, n, [])  -> do putStrLn $ "Unrecognized arguments: " ++ concat n ++ usageInfo "\nUsage: " options
                          exitFailure
        (_, _, es)  -> do putStrLn $ concat es ++ usageInfo "\nUsage:" options
                          exitFailure

    when (o_help opts) $ do putStrLn $ usageInfo "Usage:" options

    let f = o_firstParamToTake opts
    let l = o_lastParamToTake opts
    let s = o_startingDataSize opts
    let e = o_endingDataSize opts
    let z = if o_dataSizeStep opts /= 0 then o_dataSizeStep opts
                                        else 1000

    let dataSizeSteps = if ( (e - s) `mod` z == 0) then [s, s + z..e]
                                                   else let numberOfSteps = floor $ (fromIntegral $ e - s) / (fromIntegral z)
                                                            lastStep = s + z * numberOfSteps
                                                        in [s, (s + z)..lastStep] ++ [e] 
    let usedParams = take (l-f+1) $ drop (f - 1) params

    workingPairs <- spawnWorkingPairs usedParams dataSizeSteps
    
    mapM runTest workingPairs

    putStrLn $ "Done."
