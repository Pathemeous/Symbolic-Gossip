\section{Benchmarks}\label{sec:Benchmarks}

The primary motivation for using symbolic model checking is to provide faster computation,
as explicit model checking in DEL is generally slow even for small examples \cite{GattingerThesis2018}.

We therefore benchmark the runtime of the various implementations and compare them.
Comparing the resuls, we can find what parts of the knowledge structure or updates on it cause the slowdown.

We execute three different call sequences, dependent on the number of agents: with a higher number of agents, we use call sequences in which more agents participate. This prevents situations in which a model containing five agents is only tested on a call sequence that concerns only a small subset of those agents, which could skew the results of the tests for models with a large number of agents.
\begin{code}
module Main where

import Criterion.Main
import SimpleTransformer
import OptimizedTransformer
import Transparent
import SMCDEL.Symbolic.S5
import SMCDEL.Examples.GossipS5
import SMCDEL.Language

{-
    This module benchmarks the various transformers.
    Currently we compare
    - the SimpleTransformer (SmpTrf)
    - the ClassicTransformer in Transparent setting (TnsTrf)
    - the ClassicTransformer using the SMCDEL optimization function (OptTrf)
    - the ClasicTransformer (ClsTrf)
    The program runs updates in various settings (3,4,5 agents and 1,2,3 calls)

    * Running the Benchmark
    To run the benchmark, execute `stack bench` from the root of the project
-}

-- The call sequences we apply
-- The number parameter denotes the amount of agents involved
-- The length (up to 100) can be determined with `take`
callsequence :: Int -> [(Int, Int)]
callsequence 3 = concat $ replicate 10  [(0,1),(1,2),(1,2),(0,2),(1,2),(0,1),(0,2),(1,2),(1,2),(1,2)]
callsequence 4 = concat $ replicate 10  [(0,1),(1,2),(0,2),(2,3),(1,3),(0,2),(0,1),(1,2),(2,3),(1,3)]
callsequence 5 = concat $ replicate 10  [(0,1),(1,2),(0,2),(3,4),(1,4),(0,1),(1,3),(0,2),(3,4),(2,4)]
callsequence 15 = concat $ replicate 10 [(0,2),(3,5),(1,14),(7,8),(5,7),(3,14),(0,12),(3,8),(3,12),(7,12)]
callsequence _ = []

-- The functions we're benchmarking.
-- the scene is evaluated to ensure it is fully computed (similarly to `nf`)
-- Simple Transformer
benchSmpTrf :: Int -> Int -> Bool
benchSmpTrf a c = evalViaBdd (afterSimple a $ take c $ callsequence a) (K "0" $ allExperts a)

-- Classic Transformer
benchClsTrf :: Int -> Int -> Bool
benchClsTrf a c = evalViaBdd (after a $ take c $ callsequence a) (K "0" $ allExperts a)

-- Optimized Transformer
benchOptTrf :: Int -> Int -> Bool
benchOptTrf a c = evalViaBdd (afterOpt a $ take c $ callsequence a) (K "0" $ allExperts a)

-- Transparent Transformer
benchTnsTrf :: Int -> Int -> Bool
benchTnsTrf a c = evalViaBdd (afterTransparent a $ take c $ callsequence a) (K "0" $ allExperts a)

-- Our benchmark harness.
main :: IO ()
main = defaultMain [
  bgroup "SmpTrf - 3 agents"    [ bench "1 call"   $ whnf (benchSmpTrf 3) 1
                                , bench "3 calls"  $ whnf (benchSmpTrf 3) 3
                                , bench "5 calls"  $ whnf (benchSmpTrf 3) 5
                                , bench "15 calls" $ whnf (benchSmpTrf 3) 15
                                , bench "25 calls" $ whnf (benchSmpTrf 3) 25
                                ],
  bgroup "SmpTrf - 4 agents"    [ bench "1 call"   $ whnf (benchSmpTrf 4) 1
                                , bench "3 calls"  $ whnf (benchSmpTrf 4) 3
                                , bench "5 calls"  $ whnf (benchSmpTrf 4) 5
                                , bench "15 calls" $ whnf (benchSmpTrf 4) 15
                                , bench "25 calls" $ whnf (benchSmpTrf 4) 25
                                ],
  bgroup "SmpTrf - 5 agents"    [ bench "1 call"   $ whnf (benchSmpTrf 5) 1
                                , bench "3 calls"  $ whnf (benchSmpTrf 5) 3
                                , bench "5 calls"  $ whnf (benchSmpTrf 5) 5
                                , bench "15 calls" $ whnf (benchSmpTrf 5) 15
                                , bench "25 calls" $ whnf (benchSmpTrf 5) 25
                                , bench "50 calls" $ whnf (benchSmpTrf 5) 50
                                , bench "75 calls" $ whnf (benchSmpTrf 5) 75
                                , bench "100 calls" $ whnf (benchSmpTrf 5) 100
                                ],
  bgroup "SmpTrf - 15 agents"   [ bench " 5 call"  $ whnf (benchSmpTrf 15) 5
                                , bench "15 calls" $ whnf (benchSmpTrf 15) 15
                                , bench "25 calls" $ whnf (benchSmpTrf 15) 25
                                , bench "50 calls" $ whnf (benchSmpTrf 15) 50
                                , bench "75 calls" $ whnf (benchSmpTrf 15) 75
                                , bench "100 calls" $ whnf (benchSmpTrf 15) 100
                                ],
  bgroup "TnsTrf - 3 agents"    [ bench "1 call"   $ whnf (benchTnsTrf 3) 1
                                , bench "3 calls"  $ whnf (benchTnsTrf 3) 3
                                , bench "5 calls"  $ whnf (benchTnsTrf 3) 5
                                ],
  bgroup "TnsTrf - 4 agents"    [ bench "1 call"   $ whnf (benchTnsTrf 4) 1
                                , bench "3 calls"  $ whnf (benchTnsTrf 4) 3
                                , bench "5 calls"  $ whnf (benchTnsTrf 4) 5
                                ],
  bgroup "TnsTrf - 5 agents"    [ bench "1 call"   $ whnf (benchTnsTrf 5) 1
                                , bench "3 calls"  $ whnf (benchTnsTrf 5) 3
                                , bench "5 calls"  $ whnf (benchTnsTrf 5) 5
                                , bench "15 calls" $ whnf (benchTnsTrf 5) 15
                                , bench "25 calls" $ whnf (benchTnsTrf 5) 25
                                , bench "50 calls" $ whnf (benchTnsTrf 5) 50
                                , bench "75 calls" $ whnf (benchTnsTrf 5) 75
                                ],
  bgroup "TnsTrf - 15 agents"   [ bench "1 call"   $ whnf (benchTnsTrf 15) 1
                                , bench "3 calls"  $ whnf (benchTnsTrf 15) 3
                                , bench "5 calls"  $ whnf (benchTnsTrf 15) 5
                                , bench "15 calls" $ whnf (benchTnsTrf 15) 15
                                , bench "25 calls" $ whnf (benchTnsTrf 15) 25
                                ],
  bgroup "OptTrf - 3 agents"    [ bench "1 call"   $ whnf (benchOptTrf 3) 1
                                , bench "3 calls"  $ whnf (benchOptTrf 3) 3
                                , bench "5 calls"  $ whnf (benchOptTrf 3) 5
                                ],
  bgroup "OptTrf - 4 agents"    [ bench "1 call"   $ whnf (benchOptTrf 4) 1
                                , bench "3 calls"  $ whnf (benchOptTrf 4) 3
                                , bench "5 calls"  $ whnf (benchOptTrf 4) 5
                                ],
  bgroup "OptTrf - 5 agents"    [ bench "1 call"   $ whnf (benchOptTrf 5) 1
                                , bench "3 calls"  $ whnf (benchOptTrf 5) 3
                                -- no case for 5 calls as it runs 45+ mins without result
                                ],
  bgroup "ClsTrf - 3 agents"    [ bench "1 call"   $ whnf (benchClsTrf 3) 1
                                , bench "3 calls"  $ whnf (benchClsTrf 3) 3
                                , bench "5 calls"  $ whnf (benchClsTrf 3) 5
                                ],
  bgroup "ClsTrf - 4 agents"    [ bench "1 call"   $ whnf (benchClsTrf 4) 1
                                , bench "3 calls"  $ whnf (benchClsTrf 4) 3
                                , bench "5 calls"  $ whnf (benchClsTrf 4) 5
                                ],
  bgroup "ClsTrf - 5 agents"    [ bench "1 call"   $ whnf (benchClsTrf 5) 1
                                , bench "3 calls"  $ whnf (benchClsTrf 5) 3
                                , bench "5 calls"  $ whnf (benchClsTrf 5) 5
                                ]
  ]
\end{code}
