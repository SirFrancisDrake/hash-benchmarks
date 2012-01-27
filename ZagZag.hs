module ZagZag
  ( zagZag
  , makeTuples
  , makeTriples
  , getRandomSublist
  )
  where

import Control.Applicative ((<$>))
import DataGenerator
import Data.List (delete)
import System.Random

lokTar cmd args = map (\arg -> (cmd, [arg])) args
zagZag cmds args = concatMap (\cmd -> lokTar cmd args) cmds

makeTuples :: [a] -> [b] -> [(a, b)]
makeTuples xs ys = map (\t -> (fst t, head $ snd t)) $ zagZag xs ys

makeTriples :: [a] -> [b] -> [c] -> [(a,b,c)]
makeTriples xs ys zs = map untriple $ makeTuples xs $ makeTuples ys zs

untriple (a,(b,c)) = (a,b,c)

getRandomSublist :: (Eq a) => Int -> [a] -> IO [a]
getRandomSublist 0 _ = return []
getRandomSublist _ [] = return []
getRandomSublist i xs = do
    whichToTake <- getStdRandom $ randomR (0, (length xs) -1)
    let elemTaken = xs !! whichToTake
    (elemTaken:) <$> (getRandomSublist (i-1) (delete elemTaken xs))

