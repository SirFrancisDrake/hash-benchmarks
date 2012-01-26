module ZagZag
  ( zagZag
  , makeTuples
  , makeTriples
  )
  where

import DataGenerator

lokTar cmd args = map (\arg -> (cmd, [arg])) args
zagZag cmds args = concatMap (\cmd -> lokTar cmd args) cmds

makeTuples :: [a] -> [b] -> [(a, b)]
makeTuples xs ys = map (\t -> (fst t, head $ snd t)) $ zagZag xs ys

makeTriples :: [a] -> [b] -> [c] -> [(a,b,c)]
makeTriples xs ys zs = map untriple $ makeTuples xs $ makeTuples ys zs

untriple (a,(b,c)) = (a,b,c)
