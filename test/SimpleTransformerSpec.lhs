\subsection{Simple Transformer} \label{ssec:SimpleTests}

The Simple Transformer does not satisfy the same formulas as the Classic Transformer (and the transparent variant): 
some instances of higher-order knowledge fail. The following tests show how the Simple Transformer differs 
from the other two. 

\begin{code}
module SimpleTransformerSpec where

import SimpleTransformer

import Test.Hspec hiding ( after )
import SMCDEL.Examples.GossipS5
import SMCDEL.Symbolic.S5
import SMCDEL.Language
\end{code}

We test the implementation of the Simple Transformer with the following tests. As with the transparent variant, 
the first five tests encode the basic requirements of a transformer for a Gossip problem. 

\begin{enumerate}
    \item For agents $a,b$: in the initial model, $a$ knows that $b$ doesn't know $a$'s secret 
    \item For agents $a,b$: after call ab, $a$ knows $b$'s secret 
    \item For agents $a,b,c$: after call sequence [$ab,bc$], $c$ knows $a$'s secret 
    \item For agents $a,b,c$: after one call, there should be no experts
    \item For agents $a,b,c$: after call sequence [$ab,bc,ca$], everyone should be an expert

    \item For agents $a,b,c$: after call $ab$, $c$ knows that $a$ knows $b$'s secret 
    \item For agents $a,b,c,d$: after call sequence [$ab,bc$], $d$ knows that $c$ knows $a$'s secret 
    \item For agents $a,b,c$: after call sequence [$ab,bc,ca$], everyone should know that everyone's an expert 

    \item For agents $a,b$: after call $ab$, $b$ knows that $a$ knows $b$'s secret 
    \item For agents $a,b,c,d$: after call sequence [$ab,bc,cd,ca$], $a$ knows that $d$ knows $a$'s secret
    and that $d$ knows that $c$ knows $a$'s secret 
\end{enumerate} 

\begin{code}
spec :: Spec
spec = do
        -- simple tests (same tests as those for the transparent implementation)
        it "simpTrf 1: no faulty knowledge in initial model" $ do 
            eval (simpleGossipInit 2) (K "0" (Neg (has 2 1 0))) `shouldBe` True
        it "simpTrf 2: call shares secrets between agents" $ do 
            eval (afterSimple 2 [(0,1)]) (Conj [has 2 1 0, has 2 0 1]) `shouldBe` True 
        it "simpTrf 3: call sequence shares secrets between agents" $ do 
            eval (afterSimple 3 [(0,1),(1,2)]) (has 3 2 0) `shouldBe` True 
        it "simpTrf 4: no faulty experts" $ do 
            eval (afterSimple 3 [(0,1)]) (Disj [expert 3 i | i <- [0..3]]) `shouldBe` False 
        it "simpTrf 5: all are experts after the correct call sequence" $ do 
            isSuccessSimple 3 [(0,1),(1,2),(2,0)] `shouldBe` True

        -- simple-trf-specific tests: these might fail but we'd like them to be true -- CHECK THIS 
        it "simpTrf 6: call is observed by other agents" $ do 
            eval (afterTransparent 3 [(0,1)]) (K "2" (has 3 0 1)) `shouldBe` True 
        it "simpTrf 7: call sequence is observed by other agents" $ do 
            eval (afterTransparent 4 [(0,1),(1,2)]) (K "3" (has 3 2 0)) `shouldBe` True 
        it "simpTrf 8: all agents know that all are experts after the correct call sequence" $ do 
            eval (afterTransparent 3 [(0,1),(1,2),(2,0)]) (Conj [ K (show i) (allExperts 3) 
                                                                | i <- [(0::Int)..3] ]) `shouldBe` True
        -- general higher-order knowledge tests
        it "simpTrf 9: higher-order knowledge after one call" $ do 
            eval (afterTransparent 3 [(0,1)]) (K "1" (has 3 0 1)) `shouldBe` True 
        it "simpTrf 10: higher-order knowledge after call sequence" $ do
            eval (afterTransparent 3 [(0,1),(1,2),(2,3),(2,0)]) (K "0" $ Conj [has 3 3 0, K "3" (has 3 2 0)]) `shouldBe` True
\end{code}


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
