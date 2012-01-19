

import System.IO
import System.Process
import DataGenerator

testNames = ["generator", "data-map"]
dataVariants = map show [0..11]
dataSizes = map show [10^4, 3*10^4]

main = do
  let runs = sequence [testNames, dataVariants, dataSizes]
  mapM_ run runs

run args = do
  let cmd = (proc "./Test"
          $ args ++ ["+RTS", "-t", "--machine-readable"])
          {std_out = CreatePipe, std_err = CreatePipe}
  (_,_,Just err, p) <- createProcess cmd
  waitForProcess p
  out <- hGetContents err
  putStrLn $ show args ++ ": " ++ parseRTS out


parseRTS = (!!4) . lines


