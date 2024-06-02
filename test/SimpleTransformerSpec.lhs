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
        -- Secret exchange tests (hold for sync and transparent)
        it "Secrets: all are experts after the correct call sequence" $ do
            isSuccessSimple 3 [(0,1),(1,2),(0,2)] `shouldBe` True
        it "Secrets: call shares secrets between agents" $ do
            eval (afterSimple 4 [(0,1)]) (Conj [has 4 1 0, has 4 0 1]) `shouldBe` True
        it "Secrets: secrets from first call get exchanged to second call" $ do
            eval (afterSimple 4 [(0,1),(1,2)]) (has 4 2 0) `shouldBe` True
        it "Secrets: agent of first call does not learn secrets from second call" $ do
            eval (afterSimple 4 [(0,1),(1,2)]) (has 4 0 2) `shouldBe` False
        it "Secrets: no faulty experts" $ do
            eval (afterSimple 3 [(0,1)]) (Disj [expert 3 i | i <- [0..2]]) `shouldBe` False

        -- Tests about more direct knowledge
        it "Knowledge: initial knowledge of own secret atoms" $ do
            eval (gossipInitSimple 3) (K "0" (Neg (has 3 0 1))) `shouldBe` True
        it "Knowledge: agent knows callee knows their secret after call" $ do
            eval (afterSimple 4 [(0,1),(1,2)]) (K "2" (has 4 1 2)) `shouldBe` True
        it "Knowledge: agent knows knowledge of other agent in same call" $ do
            eval (afterSimple 4 [(0,1),(1,2)]) (K "2" (has 4 1 0)) `shouldBe` True
        it "Knowledge: all agents know that all are experts after the explicit call sequence" $ do
            eval (afterSimple 3 [(0,1),(1,2),(0,2),(0,1),(1,2),(0,2)]) (Conj [ K (show i) (allExperts 3)
                                                                | i <- [(0::Int)..2] ]) `shouldBe` True

        -- Tests about inferred knowledge
        it "Knowledge (inferred): initial inferred knowledge of other's secret atoms" $ do
            eval (gossipInitSimple 3) (K "0" (Neg (has 3 1 0))) `shouldBe` True
        it "Knowledge (inferred): third agent infers knowledge of first agent after 2 calls (4 agents)" $ do
            eval (afterSimple 4 [(0,1),(1,2)]) (K "2" (has 4 0 1)) `shouldBe` True
        it "Knowledge (inferred): non-involed knows what call happened (3 agents)" $ do
            eval (afterSimple 3 [(0,1)]) (K "2" (has 3 0 1)) `shouldBe` True
        it "Knowledge (inferred): agents can reason about the limits of other agents' knowledge" $ do
            eval (afterSimple 3 [(0,1)]) (K "0" (Neg (has 3 2 1))) `shouldBe` True
        it "Knowledge (inferred): all agents infer they are all experts after minimal call sequence (3 agents)" $ do
            eval (afterSimple 3 [(0,1),(1,2),(0,2)]) (Conj [K (show i) (allExperts 3)
                                                                | i <- [(0::Int)..2]]) `shouldBe` True

        -- transparent-specific knowledge
        it "Knowledge (transparent): call is observed by other agents should fail" $ do
            eval (afterSimple 4 [(0,1)]) (K "2" (has 3 0 1)) `shouldBe` False
        it "Knowledge (transparent): call sequence is observed by non-involed agents should fail" $ do
            eval (afterSimple 4 [(0,1),(1,2)]) (K "3" (has 3 2 0)) `shouldBe` False
\end{code}

% fixme:
% - run these tests
% - change ones that fail to "should be false" and make a comment above those
% - and write a sentence or two about them here

% old text:
% Note in particular the test \texttt{SmpTrf: higher-order knowledge fails}, which returns false.
% However, the tested formula $K_2 S_01$ should be true after calls $01;12$: after the second call,
% agent $2$ should be able to infer that the prior call was between agents $0$ and $1$ and conclude that their secrets were exchanged.
