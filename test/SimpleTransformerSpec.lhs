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

We test the implementation of the Simple Transformer with the following tests.
The first four tests (explained below) describe instances of higher-order knowledge and aren't
all satisfied by the Simple Transformer, even though they should be. % check this
The other tests are identical instances from the transparent test. from As with the transparent variant,
tests 5-9 encode the basic requirements of a transformer for a Gossip problem and 10-11 encode general instances of
higher-order knowledge.

New tests:
\begin{enumerate}
    \item For agents $a,b,c,d$: after call sequence [$ab,bc$], $c$ knows that $a$ knows that
    $b$ knows $a$'s secret
    \item For agents $a,b,c$: after call $ab$, $c$ can infer that $a$ knows $b$'s secret (since there was only one possible call)
    \item For agents $a,b,c$: after call sequence [$ab$], $a$ knows that $c$ doesn't know $b$'s secret
    \item For agents $a,b,c$: after call sequence [$ab,bc,ca,ab,bc,ca$], everyone should know that everyone's an expert
\end{enumerate}

\begin{code}
spec :: Spec
spec = do
        -- simple-trf-specific tests: these might fail but we'd like them to be true -- CHECK THIS
        it "simpTrf 1: agents can reason about other agents' knowledge 1" $ do
            eval (afterSimple 4 [(0,1),(1,2)]) (K "2" (has 4 1 0)) `shouldBe` True
        it "simpTrf 2: three agents non-involed knows what call happened" $ do
            eval (afterSimple 3 [(0,1)]) (K "2" (has 3 0 1)) `shouldBe` False -- LIMITATIONS of SimpleTrf
        it "simpTrf 3: agents can reason about the limits of other agents' knowledge" $ do
            eval (afterSimple 3 [(0,1)]) (K "0" (Neg (has 3 2 1))) `shouldBe` False -- LIMITATIONS of SimpleTrf
        it "simpTrf 4: all agents know that all are experts after the correct call sequence" $ do
            eval (afterSimple 3 [(0,1),(1,2),(0,2),(0,1),(1,2),(0,2)]) (Conj [ K (show i) (allExperts 3)
                                                                | i <- [(0::Int)..2] ]) `shouldBe` True
        -- simple tests (same tests as those for the transparent implementation)
        it "simpTrf 5: knowledge of initial state" $ do
            eval (gossipInitSimple 2) (K "0" (Neg (has 2 1 0))) `shouldBe` True
        it "simpTrf 6: call shares secrets between agents" $ do
            eval (afterSimple 2 [(0,1)]) (Conj [has 2 1 0, has 2 0 1]) `shouldBe` True
        it "simpTrf 7: call sequence shares secrets between agents" $ do
            eval (afterSimple 3 [(0,1),(1,2)]) (has 3 2 0) `shouldBe` True
        it "simpTrf 8: no faulty experts" $ do
            eval (afterSimple 3 [(0,1)]) (Disj [expert 3 i | i <- [0..2]]) `shouldBe` False
        it "simpTrf 9: all are experts after the correct call sequence" $ do
            isSuccessSimple 3 [(0,1),(1,2),(0,2)] `shouldBe` True

        -- other general higher-order knowledge tests (same tests as those for the transparent implementation)
        it "simpTrf 10: higher-order knowledge after one call" $ do
            eval (afterSimple 3 [(0,1)]) (K "1" (has 3 0 1)) `shouldBe` True
        -- it "simpTrf 11: higher-order knowledge after call sequence" $ do
        --     eval (afterSimple 3 [(0,1),(1,2),(2,3),(0,2)]) (K "0" $ Conj [has 3 3 0, K "3" (has 3 2 0)]) `shouldBe` True
\end{code}

% fixme:
% - run these tests
% - change ones that fail to "should be false" and make a comment above those
% - and write a sentence or two about them here

% old text:
% Note in particular the test \texttt{SmpTrf: higher-order knowledge fails}, which returns false.
% However, the tested formula $K_2 S_01$ should be true after calls $01;12$: after the second call,
% agent $2$ should be able to infer that the prior call was between agents $0$ and $1$ and conclude that their secrets were exchanged.
