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
import Options
import TestData
import ZagZag

type HashTable k v = H.BasicHashTable k v
type HashChair = HashTable B.ByteString B.ByteString
type HashTuple = (B.ByteString, B.ByteString)

runTestWithData :: [TestPair] -> Int -> IO String -- IO Performance info
runTestWithData vals random = do
    time <- getCurrentTime
    ht <- genHashtable vals
    evaluate ht
    setTime <- (flip diffUTCTime time) <$> getCurrentTime

    rndQueryTime <- randomQueryHashtable ht random

    time <- getCurrentTime
    hresult <- queryHashtable ht
    evaluate (rnf hresult)
    getTime <- (flip diffUTCTime time) <$> getCurrentTime

    return $ "Basic Hashtable results\nSet time: " ++ show setTime ++ "\nGet time: " 
             ++ show getTime ++ "\nAverage random query time over " ++ show random ++
             " samples is: " ++ show (rndQueryTime / (fromIntegral random))

genHashtable :: [TestPair] -> IO (HashTable B.ByteString B.ByteString)
genHashtable vals = H.fromList vals

queryHashtable :: (HashTable B.ByteString B.ByteString) -> IO [HashTuple]
queryHashtable ht = H.toList ht

randomQueryHashtable :: (HashTable B.ByteString B.ByteString) -> Int -> IO NominalDiffTime
randomQueryHashtable ht rnd = do
    time <- getCurrentTime
    pairs <- queryHashtable ht
    let keys = map fst pairs
    keysToRequest <- getRandomSublist rnd keys
    answers <- map fromJust <$> mapM (H.lookup ht) keysToRequest
    evaluate (rnf answers)
    (flip diffUTCTime time) <$> getCurrentTime

runTest :: (Params, [TestPair]) -> Int -> IO ()
runTest (pr, tps) rnd = putStrLn ("\n" ++ (show pr) ++ "\nNumber of test pairs: " ++ show (length tps)) >> runTestWithData tps rnd >>= putStrLn

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
    let r = o_randomSamples opts

    let dataSizeSteps = if ( (e - s) `mod` z == 0) then [s, s + z..e]
                                                   else let numberOfSteps = floor $ (fromIntegral $ e - s) / (fromIntegral z)
                                                            lastStep = s + z * numberOfSteps
                                                        in [s, (s + z)..lastStep] ++ [e] 
    let usedParams = take (l-f+1) $ drop (f - 1) params

    workingPairs <- spawnWorkingPairs usedParams dataSizeSteps
    
    mapM (\p -> runTest p r) workingPairs

    putStrLn $ "Done."
