module Bench.TrfBenchmarks where

import Criterion.Main
import SimpleTransformer
import SMCDEL.Symbolic.S5
import SMCDEL.Examples.GossipS5
import SMCDEL.Language

{-
    This module benchmarks the various transformers.
    Currently we compare
    - the SimpleTransformer (SmpTrf)
    - the ClasicTransformer (ClsTrf)
    The program runs updates in various settings (3,4,5 agents and 1,2,3 calls)

    * Runnng the Benchmark
    To run the benchmark, do `stack build`. Then run

        stack ghci bench/TrfBenchmarks.hs

    When in the ghci, run:

        main --time-limit=60

    The time limit parameter is a Criterion CLI argument
    that is required because the benchmarks otherwise are shorted after 1 second
-}

-- The call sequence we apply
callsequence :: [(Int, Int)]
callsequence  = [(0,1),(1,2),(0,2)]

-- The function we're benchmarking.
-- Simple Transformer
benchSmpTrf :: Int -> Int -> Bool
benchSmpTrf a c = evalViaBdd (afterSimple a $ take c callsequence) (K "0" $ allExperts a)

-- Classic Transformer
benchClsTrf :: Int -> Int -> Bool
benchClsTrf a c = evalViaBdd (after a $ take c callsequence) (K "0" $ allExperts a)

-- Our benchmark harness.
main :: IO ()
main = defaultMain [
  bgroup "SmpTrf - 3 agents"    [ bench "1 call"   $ whnf (benchSmpTrf 3) 1
                                , bench "2 calls"  $ whnf (benchSmpTrf 3) 2
                                , bench "3 calls"  $ whnf (benchSmpTrf 3) 3
                                ],
  bgroup "SmpTrf - 4 agents"    [ bench "1 call"   $ whnf (benchSmpTrf 4) 1
                                , bench "2 calls"  $ whnf (benchSmpTrf 4) 2
                                , bench "3 calls"  $ whnf (benchSmpTrf 4) 3
                                ],
  bgroup "SmpTrf - 5 agents"    [ bench "1 call"   $ whnf (benchSmpTrf 5) 1
                                , bench "2 calls"  $ whnf (benchSmpTrf 5) 2
                                , bench "3 calls"  $ whnf (benchSmpTrf 5) 3
                                ],
  bgroup "ClsTrf - 3 agents"    [ bench "1 call"   $ whnf (benchClsTrf 3) 1
                                , bench "2 calls"  $ whnf (benchClsTrf 3) 2
                                , bench "3 calls"  $ whnf (benchClsTrf 3) 3
                                ],
  bgroup "ClsTrf - 4 agents"    [ bench "1 call"   $ whnf (benchClsTrf 4) 1
                                , bench "2 calls"  $ whnf (benchClsTrf 4) 2
                                , bench "3 calls"  $ whnf (benchClsTrf 4) 3
                                ],
  bgroup "ClsTrf - 5 agents"    [ bench "1 call"   $ whnf (benchClsTrf 5) 1
                                , bench "2 calls"  $ whnf (benchClsTrf 5) 2
                                , bench "3 calls"  $ whnf (benchClsTrf 5) 3
                                ]
  ]