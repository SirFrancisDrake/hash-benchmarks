import System.IO
import System.Process
import DataGenerator

testNames = ["HashtablesTest", "MapTest"]
dataSizes = map show [10^4, 10^5, 10^6]
clients = map show [1,2,3,4,5]

main = mapM_ run (makeTuples testNames dataSizes) >>
       mapM_ (\d -> runRedis d clients) dataSizes

zagZag cmd args = map (\arg -> (cmd, [arg])) args
lokTar cmds args = concatMap (\cmd -> zagZag cmd args) cmds

makeTuples :: [String] -> [String] -> [(String, [String])]
makeTuples = lokTar 

runRedis :: String -> [String] -> IO ()
runRedis args [] = runRedis args ["1"]
runRedis args clients = run ("./RedisTest", args: clients)

run (acmd, args) = do
  let cmd = (proc ("./" ++ acmd)
          $ args ++ ["+RTS", "-t", "-s", "--machine-readable"])
          {std_out = CreatePipe, std_err = CreatePipe}
  (_,Just stdout,Just err, p) <- createProcess cmd
  waitForProcess p
  output <- hGetContents stdout
  error <- hGetContents err
  putStrLn $ show acmd ++ " | " ++ show args ++ ":\n" ++ output ++ parseRTS error

parseRTS out = unlines $ map (lns!!) [6]
  where
    lns = lines out
