\subsection{Simple Transformer} \label{ssec:SimpleTests}


\begin{code}
module SimpleTransformerSpec where

import SimpleTransformer

import Test.Hspec hiding ( after )
import SMCDEL.Examples.GossipS5
import SMCDEL.Symbolic.S5
import SMCDEL.Language
import SmpTrfS5
\end{code}

We test the implementation of the Simple Transformer with the following tests.

simple checks: -> should also work for simple
- for agents a,b: after call ab, a knows b's secret
- for agents a,b,c: after call sequence [ab,bc], c knows a's secret
- for agents a,b,c: after one call, c should not be an expert
- for agents a,b,c: after call sequence [ab,bc,ca], everyone should be an expert

higher-order knowledge checks: -> might not work for simple
- for agents a,b,c: after call ab, c knows that a knows b's secret (inference from possible calls)
- for agents a,b: after call ab, b knows that a knows b's secret
- for agents a,b,c,d: after call sequence [ab,bc,cd,ca], a knows that d knows a's secret


\begin{code}
spec :: Spec
spec = do
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
