-- schrijf intro hier

\begin{code}
module ClassicTransformerSpec where

import Test.Hspec hiding ( after )
import SMCDEL.Examples.GossipS5
import SMCDEL.Language
import SMCDEL.Symbolic.S5
\end{code}

In particular, we use the following functions (previously defined in \cite{GattingerThesis2018}) concerning experts
\footnote{An expert is an agent who knows all secrets, that is, \texttt{expert n a} is defined as 
$\bigwedge \{S_a b\mid b\in [1,...,n]\}$}, 
which define the formulas "agent a is an expert" and "all agents are experts": 

\begin{verbatim}
    expert :: Int -> Int -> Form
    expert n a = Conj [ PrpF (hasSof n a b) | b <- gossipers n, a /= b ]

    allExperts :: Int -> Form
    allExperts n = Conj [ expert n a | a <- gossipers n ]
\end{verbatim}

simple checks: 
- for agents a,b: after call ab, a knows b's secret 
- for agents a,b,c: after call sequence [ab,bc], c knows a's secret 
- for agents a,b,c: after one call, c should not be an expert 
- for agents a,b,c: after call sequence [ab,bc,ca], everyone should be an expert
 
higher-order knowledge checks: 
- for agents a,b,c: after call ab, c knows that a knows b's secret (inference from possible calls)
- for agents a,b: after call ab, b knows that a knows b's secret 
- for agents a,b,c,d: after call sequence [ab,bc,cd,ca], a knows that d knows a's secret 


We can verify that $K_2 S_01$ is indeed true after the calls $01;12$ in the classic transformer.

\begin{code}
spec :: Spec
spec = do
        it "clsTrf: second call shares secrets of other agents" $ do
            eval (after 3 [(0,1),(1,2)]) (K "2" $ has 3 0 1) `shouldBe` True
        it "clsTrf: second call shares secrets of other agents" $ do
            eval (after 3 [(0,1),(1,2)]) (K "2" $ has 3 0 1) `shouldBe` True
        it "clsTrf: 3 agents 1 call should infer knowledge" $ do
            eval (after 3 [(0,1)]) (K "2" $ has 3 0 1) `shouldBe` True
\end{code}

Indeed this test passes, highlighting the limitations of our earlier simple transformer.