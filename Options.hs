module Options
  ( Opt(..)
  , defFirstParam
  , defLastParam
  , defStartingDataSize
  , defEndingDataSize
  , defDataSizeStep
  , defThreads
  , defOpts
  , options
  )
  where

import System (getArgs)
import System.Console.GetOpt

data Opt = Opt { o_firstParamToTake :: Int
               , o_lastParamToTake :: Int
               , o_startingDataSize :: Int
               , o_endingDataSize :: Int
               , o_dataSizeStep :: Int
               , o_threads :: Int
               , o_help :: Bool
               }
           deriving Show

defOpts = Opt defFirstParam defLastParam defStartingDataSize defEndingDataSize defDataSizeStep defThreads False
defFirstParam = 1
defLastParam = 1
defStartingDataSize = 10000
defEndingDataSize = 10000
defDataSizeStep = 100
defThreads = 1

options :: [OptDescr (Opt -> Opt)]
options = [Option ['f'] ["firstParamToTake"] (OptArg (maybe id (\f o -> o{o_firstParamToTake = read f})) "firstParamToTake") ("First param to be tested (default " ++ show defFirstParam ++ ")"),
           Option ['l'] ["lastParamToTake"] (OptArg (maybe id (\l o -> o{o_lastParamToTake = read l})) "lastParamToTake") ("Amount of params to be tested (default " ++ show defLastParam ++ ")"),
           Option ['s'] ["startingDataSize"] (OptArg (maybe id (\s o -> o{o_startingDataSize = read s})) "startingDataSize") ("Data size to start with (default " ++ show defStartingDataSize ++ ")"),
           Option ['e'] ["endingDataSize"] (OptArg (maybe id (\d o -> o{o_endingDataSize = read d})) "endingDataSize") ("Data size to stop at (default " ++ show defEndingDataSize ++ ")"),
           Option ['z'] ["dataSizeStep"] (OptArg (maybe id (\z o -> o{o_dataSizeStep = read z})) "dataSizeStep") ("Data size is stepped by (default " ++ show defDataSizeStep ++ ")"),
           Option ['t'] ["threads"] (OptArg (maybe id (\t o -> o{o_threads = read t})) "threads") ("Maximum number of threads to run (minimum is always 1, default " ++ show defThreads ++ ")"),
           Option [] ["help"] (NoArg (\o -> o{o_help = True})) "Show this usage info"]
