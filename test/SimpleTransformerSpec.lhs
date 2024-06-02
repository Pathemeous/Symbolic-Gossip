\subsubsection{Simple Transformer Tests} \label{sec:SimpleTests}

\begin{code}
module SimpleTransformerSpec where

import SimpleTransformer

import Test.Hspec hiding ( after )
import SMCDEL.Examples.GossipS5
import SMCDEL.Symbolic.S5
import SMCDEL.Language


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