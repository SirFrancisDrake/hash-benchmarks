import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (when)
import qualified Data.ByteString as B
import Data.List (foldl')
import qualified Data.Map as M -- for parsing RedisInfo which is a Map
import Data.Maybe (fromJust)
import Database.Redis.Redis
import Data.Time.Clock
import System (getArgs)
import System.Console.GetOpt
import System.Exit (exitFailure, exitSuccess)
import System.IO.Unsafe

import DataGenerator
import Options
import TestData
import ZagZag

-- Эта функция -- боттлнек. Я не знаю, как решить такую проблему.
-- Естественно, боттлнек происходит до подсчёта времени
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery 0 _ = []
splitEvery i xs | (length xs <= i) = [xs]
                | otherwise = (take i xs):(splitEvery i (drop i xs))

host = localhost
port = defaultPort
db = 5

makeConnections :: Int -> String -> String -> Int -> IO [Redis]
makeConnections count host port db = mapM mkConn [0 .. (count - 1)]
    where mkConn n = do r <- connect host port
                        select r db
                        return r

waitForThreads :: MVar [ MVar () ] -> IO ()
waitForThreads threadList = do
    threads <- takeMVar threadList
    case threads of
        [] -> putMVar threadList [] >> return ()
        (t:ts) -> do
            putMVar threadList ts
            thread <- takeMVar t
            waitForThreads threadList

fork :: (Redis -> [TestPair] -> IO ()) -> Redis -> [TestPair] -> MVar [ MVar () ] -> IO ThreadId
fork fn r d threadList = do
    mvar <- newEmptyMVar
    threads <- takeMVar threadList
    putMVar threadList (mvar:threads)
    forkIO (do { fn r d >> putMVar mvar () })

runTest :: [ (Redis, [TestPair]) ] -> IO (NominalDiffTime, NominalDiffTime)
runTest pairs = do
    let threadList = unsafePerformIO (newMVar [])
    let runFn fn = \pair -> fork fn (fst pair) (snd pair) threadList

    time <- getCurrentTime
    mapM_ (runFn runPut) pairs
    waitForThreads threadList
    setTime <- (flip diffUTCTime time) <$> getCurrentTime

    time <- getCurrentTime
    mapM_ (runFn runGetAll) pairs
    waitForThreads threadList
    getTime <- (flip diffUTCTime time) <$> getCurrentTime

    return (setTime, getTime)

runPut :: Redis -> [TestPair] -> IO ()
runPut r ps = do
    sequence_ $ map (\p -> set r (fst p) (snd p)) ps

runGetAll :: Redis -> [TestPair] -> IO ()
runGetAll r ps = do
    sequence_ $ map (\p -> get r (fst p) :: IO (Reply ())) ps

testRedis :: Int -> [TestPair] -> IO String -- IO Performance Info
testRedis clients testPairs = do
    r <- connect host port
    select r db
    flushDb r

    rs <- makeConnections clients host port db
    let pairs = splitEvery (ceiling $ fromIntegral (length testPairs) / fromIntegral clients) testPairs
    -- make a note that splitEvery (ceiling $ fromIntegral (length [1..9]) / 4) [1..9] gives a reply
    -- of [[1,2,3],[4,5,6],[7,8,9]], which has all the numbers, but leaves out one connection.
    -- A correct answer would be [[1,2,3],[4,5],[6,7],[8,9]], but the appropriate function is longer

    (setTime, getTime) <- runTest $ zip rs pairs

    dbInfo <- info r
    let memory = fromJust $ M.lookup "used_memory_human" dbInfo

    mapM_ disconnect rs

    return $ "\nClients: " ++ show clients ++ "\nSet time: " ++ show setTime ++ "\nGet time: " ++ 
             show getTime ++ "\nMemory used in Redis: " ++ show memory

testRedisRandom :: Int -> [TestPair] -> [TestPair] -> IO String
testRedisRandom random getPairs setPairs = do
    r <- connect host port
    select r db

    pairsToRequest <- getRandomSublist random getPairs

    time <- getCurrentTime
    runGetAll r pairsToRequest
    rndGetTimeAmongstAll <- flip diffUTCTime time <$> getCurrentTime

    time <- getCurrentTime
    runPut r setPairs
    rndSetTimeAmongstAll <- flip diffUTCTime time <$> getCurrentTime

    return $ "Average random access time over " ++ show random ++ " tries: " ++ 
             show (rndGetTimeAmongstAll / (fromIntegral $ length pairsToRequest)) ++
             ", average insert time over " ++ show (length setPairs) ++ 
             " samples is: " ++ show (rndSetTimeAmongstAll / (fromIntegral $ length setPairs))

testRedisMultipleClients :: [Int] -> [TestPair] -> [TestPair] -> Int -> IO String -- IO Performance Info
testRedisMultipleClients clients testPairs rndSetPairs random = do
    standardTestResults <- unlines <$> mapM (\c -> testRedis c testPairs) clients
    randomTestResults <- testRedisRandom random testPairs rndSetPairs
    return $ standardTestResults ++ randomTestResults

wrappedTestRedisMultipleClients :: [Int] -> (Params, [TestPair]) -> (Params, [TestPair]) -> Int -> IO ()
wrappedTestRedisMultipleClients clients (p, tps) (_, rndSetPairs) random = 
    putStrLn ("\n" ++ show p) >> testRedisMultipleClients clients tps rndSetPairs random >>= putStrLn
        
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
    let t = o_threads opts
    let r = o_randomSamples opts

    let dataSizeSteps = if ( (e - s) `mod` z == 0) then [s, s + z..e]
                                                   else let numberOfSteps = floor $ (fromIntegral $ e - s) / (fromIntegral z)
                                                            lastStep = s + z * numberOfSteps
                                                        in [s, (s + z)..lastStep] ++ [e] 
    let usedParams = take (l-f+1) $ drop (f - 1) params

    workingPairs <- spawnWorkingPairs usedParams dataSizeSteps
    randomInsertPairs <- head <$> spawnWorkingPairs usedParams [r]
    
    mapM (\p -> wrappedTestRedisMultipleClients [1..t] p randomInsertPairs r) workingPairs

    putStrLn $ "Done."
