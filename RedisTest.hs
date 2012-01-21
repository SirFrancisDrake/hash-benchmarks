module RedisTest 
  ( testRedis
  )
  where

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (join)
import Data.List (intersperse)
import Database.Redis.Redis
import Data.Time.Clock
import System.IO.Unsafe

import TestData

-- Эта функция -- боттлнек. Я не знаю, как решить такую проблему.
-- Естественно, боттлнек происходит до подсчёта времени
splitEvery :: Int -> [a] -> [[a]]
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

threadList :: MVar [ MVar () ]
threadList = unsafePerformIO (newMVar [])

waitForThreads :: IO ()
waitForThreads = do
    threads <- takeMVar threadList
    case threads of
        [] -> return ()
        (t:ts) -> do
            putMVar threadList ts
            thread <- takeMVar t
            waitForThreads

fork :: ((Redis, [TestPair]) -> IO ()) -> (Redis, [TestPair]) -> IO ThreadId
fork fn d = do
    mvar <- newEmptyMVar
    threads <- takeMVar threadList
    putMVar threadList (mvar:threads)
    forkIO (do { fn d >> putMVar mvar () })

runTest :: [ (Redis, [TestPair]) ] -> IO (NominalDiffTime, NominalDiffTime)
runTest pairs = do
    time <- getCurrentTime
    mapM_ (\pair -> fork runPut pair) pairs
    waitForThreads
    setTime <- (diffUTCTime time) <$> getCurrentTime
    mapM_ (\pair -> fork runGet pair) pairs
    getTime <- (diffUTCTime setTime) <$> getCurrentTime
    return (setTime, getTime)

runPut :: (Redis, [TestPair]) -> IO ()
runPut (r, ps) = do
    mapM_ (\p -> set r (fst p) (snd p)) ps

runGet :: (Redis, [TestPair]) -> IO ()
runGet (r, ps) = do
    mapM_ (\p -> get r (fst p) :: IO (Reply ())) ps

testRedis :: [TestPair] -> IO String -- IO Performance Info
testRedis testPairs = do
    r <- connect host port
    flushDb r

    rs <- makeConnections clients host port db
    let pairs = splitEvery clients testPairs

    (getTime, setTime) <- runTest $ zip rs pairs

    return $ join $ intersperse "\n" [getTime, setTime]
        
