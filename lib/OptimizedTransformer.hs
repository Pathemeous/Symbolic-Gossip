module OptimizedTransformer where

import SMCDEL.Examples.GossipS5
import SMCDEL.Symbolic.S5
import SMCDEL.Language


doOptCall :: KnowScene -> (Int, Int) -> KnowScene
doOptCall start (a,b) = optimize (vocabOf start) (start `update` call (length $ agentsOf start) (a,b))

afterOpt :: Int -> [(Int,Int)] -> KnowScene
afterOpt n = foldl doOptCall (gossipInit n)