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
        -- simple tests
        it "trsTrf 1: no faulty knowledge in initial model" $ do
            eval (gossipInit 2) (K "0" (Neg (has 2 1 0))) `shouldBe` True
        it "trsTrf 2: call shares secrets between agents" $ do
            eval (afterTransparent 2 [(0,1)]) (Conj [has 2 1 0, has 2 0 1]) `shouldBe` True
        it "trsTrf 3: call sequence shares secrets between agents" $ do
            eval (afterTransparent 3 [(0,1),(1,2)]) (has 3 2 0) `shouldBe` True
        it "trsTrf 4: no faulty experts" $ do
            eval (afterTransparent 3 [(0,1)]) (Disj [expert 3 i | i <- [0..2]]) `shouldBe` False
        it "trsTrf 5: all are experts after the correct call sequence" $ do
            isSuccessTransparent 3 [(0,1),(1,2),(0,2)] `shouldBe` True

        -- transparent-specific tests
        it "trsTrf 6: call is observed by other agents" $ do
            eval (afterTransparent 3 [(0,1)]) (K "2" (has 3 0 1)) `shouldBe` True
        it "trsTrf 7: call sequence is observed by other agents" $ do
            eval (afterTransparent 4 [(0,1),(1,2)]) (K "3" (has 3 2 0)) `shouldBe` True
        it "trsTrf 8: all agents know that all are experts after the correct call sequence" $ do
            eval (afterTransparent 3 [(0,1),(1,2),(0,2)]) (Conj [ K (show i) (allExperts 3)
                                                                | i <- [(0::Int)..2] ]) `shouldBe` True
        -- general higher-order knowledge tests
        it "trsTrf 9: higher-order knowledge after one call" $ do
            eval (afterTransparent 3 [(0,1)]) (K "1" (has 3 0 1)) `shouldBe` True
        --it "trsTrf 10: higher-order knowledge after call sequence" $ do
        --     eval (afterTransparent 3 [(0,1),(1,2),(2,3),(0,2)]) (K "0" $ Conj [has 3 3 0, K "3" (has 3 2 0)]) `shouldBe` True
\end{code}

% fixme: test if these actually pass and maybe write a sentence or two about it