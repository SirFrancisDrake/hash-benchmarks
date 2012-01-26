import Control.Monad (when)
import System (getArgs)
import System.Console.GetOpt
import System.Exit (exitFailure, exitSuccess)
import System.IO
import System.Process

import DataGenerator
import Options hiding (defOpts)
import ZagZag


internalTestNames = ["HashtablesBTest", "HashtablesCTest", "HashtablesLTest", "MapTest", "HashmapTest"]
externalTestNames = ["RedisTest"]

defOpts = Opt defFirstParam defLastParam defStartingDataSize defEndingDataSize defDataSizeStep defThreads False

run :: String -> String -> IO ()
run acmd args = do
  let newArgs = words args ++ ["+RTS", "-t", "-s", "--machine-readable"]
      cmd = (proc ("./" ++ acmd)
          $ newArgs )
          {std_out = CreatePipe, std_err = CreatePipe}
  (_,Just stdout,Just err, p) <- createProcess cmd
  waitForProcess p
  output <- hGetContents stdout
  error <- hGetContents err
  putStrLn $ "\n" ++ show acmd ++ " " ++ show newArgs ++ ":\n" ++ output ++ parseRTS error

parseRTS out = unlines $ map (lns!!) [6]
  where
    lns = lines out

 --          (param index,size)
makeArgsFromTuple :: (Int, Int) -> String
makeArgsFromTuple (p,s) = 
    "-f" ++ show p ++
    " -l" ++ show p ++
    " -s" ++ show s ++
    " -e" ++ show s

 --          (param index, size, threads)
makeArgsFromTriple :: (Int, Int, Int) -> String
makeArgsFromTriple (p,s,t) = 
    "-f" ++ show p ++
    " -l" ++ show p ++
    " -s" ++ show s ++
    " -e" ++ show s ++
    " -t" ++ show t

--            command   params   sizes
runIntTestsOn :: String -> [Int] -> [Int] -> IO ()
runIntTestsOn cmd ps ss =
    let args = map makeArgsFromTuple $ makeTuples ps ss
    in mapM_ (run cmd) args

--              command   params    sizes   threads
runExtTestsOn :: String -> [Int] -> [Int] -> [Int] -> IO ()
runExtTestsOn cmd ps ss ts =
    let args = map makeArgsFromTriple $ makeTriples ps ss ts
    in mapM_ (run cmd) args

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

    let paramList = [f..l]
    let threadsList = [1..t]
    let dataSizeSteps = if ( (e - s) `mod` z == 0) 
            then [s, s + z..e]
            else let numberOfSteps = floor $ (fromIntegral $ e - s) / (fromIntegral z)
                     lastStep = s + z * numberOfSteps
                 in [s, (s + z)..lastStep] ++ [e] 

    putStrLn $ "\nRunning \nparam numbers: " ++ show paramList ++ " and \ndata sizes: " ++ show dataSizeSteps ++
                " \nin " ++ show threadsList ++ " threads \non internal tests: " ++ show internalTestNames ++
                " and \nexternal tests: " ++ show externalTestNames ++ ".\n\nGod help us all.\n"

    mapM_ (\cmd -> runIntTestsOn cmd paramList dataSizeSteps) internalTestNames
    mapM_ (\cmd -> runExtTestsOn cmd paramList dataSizeSteps threadsList) externalTestNames

    putStrLn "Done."
