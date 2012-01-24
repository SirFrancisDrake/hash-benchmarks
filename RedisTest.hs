import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (join)
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Database.Redis.Redis
import Data.Time.Clock
import System (getArgs)
import System.IO.Unsafe

import TestData

-- Эта функция -- боттлнек. Я не знаю, как решить такую проблему.
-- Естественно, боттлнек происходит до подсчёта времени
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery 0 _ = []
splitEvery i xs | (length xs <= i) = [xs]
                | otherwise = (take i xs):(splitEvery i (drop i xs))

clients = 1
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
    mapM_ (runFn runGet) pairs
    waitForThreads threadList
    getTime <- (flip diffUTCTime time) <$> getCurrentTime

    return (setTime, getTime)

runPut :: Redis -> [TestPair] -> IO ()
runPut r ps = do
    sequence_ $ map (\p -> set r (fst p) (show $ snd p)) ps

runGet :: Redis -> [TestPair] -> IO ()
runGet r ps = do
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

    return $ "\nClients: " ++ show clients ++ "\nSet time: " ++ show setTime ++ "\nGet time: " ++ show getTime ++
            "\nMemory used in Redis: " ++ show memory

testRedisMultipleClients :: [Int] -> [TestPair] -> IO String -- IO Performance Info
testRedisMultipleClients clients testPairs = 
    unlines <$> mapM (\c -> testRedis c testPairs) clients
        
main = do
    args <- getArgs
    genData (read $ head args) >>= testRedisMultipleClients (map read $ tail args) >>= putStrLn
