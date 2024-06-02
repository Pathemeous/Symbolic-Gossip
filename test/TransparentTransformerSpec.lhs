\subsection{Transparent Transformer} \label{ssec:TransparentTests}

We execute the following tests on the transparent variant of the Classic Transformer.
The simple checks also apply to the Classic Transformer and encode the basic requirements of a transformer for a Gossip problem.
However, some of the higher-order knowledge (for instance, after a call $ab$, agent $c$ should know that $a$ knows $b$'s secret,
since $c$ knows which call happened) is specific to the transparent implementation. Finally, we include a number of higher-order
knowledge tests that are not specific to the transparent variant.

\begin{code}
module TransparentTransformerSpec where

import Test.Hspec hiding ( after )
import SMCDEL.Examples.GossipS5
import SMCDEL.Language
import SMCDEL.Symbolic.S5
import Transparent (afterTransparent, isSuccessTransparent)
\end{code}

We use the following functions (previously defined in \cite{GattingerThesis2018}) concerning experts\footnote{An expert is an
agent who knows all secrets, that is, \texttt{expert n a} is defined as
$\bigwedge \{S_a b\mid b\in [1,...,n]\}$},
which define the formulas "agent a is an expert" and "all agents are experts":

\begin{verbatim}
    expert :: Int -> Int -> Form
    expert n a = Conj [ PrpF (hasSof n a b) | b <- gossipers n, a /= b ]

    allExperts :: Int -> Form
    allExperts n = Conj [ expert n a | a <- gossipers n ]

    isSuccess :: Int -> [(Int,Int)] -> Bool
    isSuccess n cs = evalViaBdd (after n cs) (allExperts n)
\end{verbatim}

We run the following tests, in this order:

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
        -- Secret exchange tests (hold for sync and transparent)
        it "Secrets: all are experts after the correct call sequence" $ do
            isSuccessTransparent 3 [(0,1),(1,2),(0,2)] `shouldBe` True
        it "Secrets: call shares secrets between agents" $ do
            eval (afterTransparent 4 [(0,1)]) (Conj [has 4 1 0, has 4 0 1]) `shouldBe` True
        it "Secrets: secrets from first call get exchanged to second call" $ do
            eval (afterTransparent 4 [(0,1),(1,2)]) (has 4 2 0) `shouldBe` True
        it "Secrets: agent of first call does not learn secrets from second call" $ do
            eval (afterTransparent 4 [(0,1),(1,2)]) (has 4 0 2) `shouldBe` False
        it "Secrets: no faulty experts" $ do
            eval (afterTransparent 3 [(0,1)]) (Disj [expert 3 i | i <- [0..2]]) `shouldBe` False

        -- Tests about more direct knowledge
        it "Knowledge: initial knowledge of own secret atoms" $ do
            eval (gossipInit 3) (K "0" (Neg (has 3 0 1))) `shouldBe` True
        it "Knowledge: agent knows callee knows their secret after call" $ do
            eval (afterTransparent 4 [(0,1),(1,2)]) (K "2" (has 4 1 2)) `shouldBe` True
        it "Knowledge: agent knows knowledge of other agent in same call" $ do
            eval (afterTransparent 4 [(0,1),(1,2)]) (K "2" (has 4 1 0)) `shouldBe` True
        it "Knowledge: all agents know that all are experts after the explicit call sequence" $ do
            eval (afterTransparent 3 [(0,1),(1,2),(0,2),(0,1),(1,2),(0,2)]) (Conj [ K (show i) (allExperts 3)
                                                                | i <- [(0::Int)..2] ]) `shouldBe` True

        -- Tests about inferred knowledge
        it "Knowledge (inferred): initial inferred knowledge of other's secret atoms" $ do
            eval (gossipInit 3) (K "0" (Neg (has 3 1 0))) `shouldBe` True
        it "Knowledge (inferred): third agent infers knowledge of first agent after 2 calls (4 agents)" $ do
            eval (afterTransparent 4 [(0,1),(1,2)]) (K "2" (has 4 0 1)) `shouldBe` True
        it "Knowledge (inferred): non-involed knows what call happened (3 agents)" $ do
            eval (afterTransparent 3 [(0,1)]) (K "2" (has 3 0 1)) `shouldBe` True
        it "Knowledge (inferred): agents can reason about the limits of other agents' knowledge" $ do
            eval (afterTransparent 3 [(0,1)]) (K "0" (Neg (has 3 2 1))) `shouldBe` True
        it "Knowledge (inferred): all agents infer they are all experts after minimal call sequence (3 agents)" $ do
            eval (afterTransparent 3 [(0,1),(1,2),(0,2)]) (Conj [K (show i) (allExperts 3)
                                                                | i <- [(0::Int)..2]]) `shouldBe` True

        -- transparent-specific knowledge
        it "Knowledge (transparent): call is observed by other agents" $ do
            eval (afterTransparent 4 [(0,1)]) (K "2" (has 3 0 1)) `shouldBe` True
        it "Knowledge (transparent): call sequence is observed by non-involed agents" $ do
            eval (afterTransparent 4 [(0,1),(1,2)]) (K "3" (has 3 2 0)) `shouldBe` True
\end{code}
