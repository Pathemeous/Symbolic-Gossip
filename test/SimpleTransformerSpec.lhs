\begin{code}
module SimpleTransformerSpec where

import SimpleTransformer

import Test.Hspec hiding ( after )
import SMCDEL.Examples.GossipS5
import SMCDEL.Language
import HaitianS5
\end{code}

We test the implementation of the Simple Transformer with the following tests.

\begin{code}
spec :: Spec
spec = do
    describe "SimpleTransformer" $ do
        it "after same result as individual calls" $ do
            afterSimple 3 [(0,1),(1,2)] `shouldBe` doSimpleCall (doSimpleCall (simpleGossipInit 3) (0,1)) (1,2)
        it "secret was received after 1 call" $ do
            eval (afterSimple 3 [(0,1),(1,2)]) (K "0" $ has 3 0 1) `shouldBe` True
        it "a knows that b now knows their secret" $ do
            eval (afterSimple 3 [(0,1),(1,2)]) (K "0" $ has 3 1 0) `shouldBe` True
        it "secret learnt in first was exchanged in second call" $ do
            eval (afterSimple 3 [(0,1),(1,2)]) (K "2" $ has 3 2 0) `shouldBe` True
        it "SmpTrf: second call shares secrets of other agents" $ do
            eval (afterSimple 3 [(0,1),(1,2)]) (K "2" $ has 3 0 1) `shouldBe` True
        it "SmpTrf: 3 agents 1 call should infer knowledge but fails" $ do
            eval (afterSimple 3 [(0,1)]) (K "2" $ has 3 0 1) `shouldBe` False
        it "SmpTrf: higher-order knowledge True" $ do
            eval (afterSimple 3 [(0,1),(1,2)]) (K "2" $ has 3 2 1) `shouldBe` True
\end{code}

Note in particular the test \texttt{SmpTrf: higher-order knowledge fails}, which returns false. 
However, the tested formula $K_2 S_01$ should be true after calls $01;12$: after the second call, agent $2$ should be able to infer that the prior call was between agents $0$ and $1$ and conclude that their secrets were exchanged.