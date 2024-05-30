module OptimizedTransformer where

import SMCDEL.Examples.GossipS5
import SMCDEL.Symbolic.S5
import SMCDEL.Language

-- fixme: we should decide whether to make this an lhs file or explain the code in another file 
-- it's so short, we might as well do the lhs since it's shorter to just show the code than to explain it

doOptCall :: KnowScene -> (Int, Int) -> KnowScene
doOptCall start (a,b) = optimize (vocabOf start) (start `update` call (length $ agentsOf start) (a,b))

afterOpt :: Int -> [(Int,Int)] -> KnowScene
afterOpt n = foldl doOptCall (gossipInit n)