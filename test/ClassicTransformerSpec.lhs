


\begin{code}
module ClassicTransformerSpec where

import Test.Hspec hiding ( after )
import SMCDEL.Examples.GossipS5
import SMCDEL.Symbolic.S5
import SMCDEL.Language

spec :: Spec
spec = do
    -- simple-trf-specific tests: these might fail but we'd like them to be true -- CHECK THIS
    it "ClsTrf 1: agents can reason about other agents' knowledge 1" $ do
        eval (after 4 [(0,1),(1,2)]) (K "2" (has 4 1 0)) `shouldBe` True
    it "ClsTrf 2: three agents non-involed knows what call happened" $ do
        eval (after 3 [(0,1)]) (K "2" (has 3 0 1)) `shouldBe` True
    it "ClsTrf 3: agents can reason about the limits of other agents' knowledge" $ do
        eval (after 3 [(0,1)]) (K "0" (Neg (has 3 2 1))) `shouldBe` True
    it "ClsTrf 4: all agents know that all are experts after the correct call sequence" $ do
        eval (after 3 [(0,1),(1,2),(0,2),(0,1),(1,2),(0,2)]) (Conj [ K (show i) (allExperts 3)
                                                            | i <- [(0::Int)..2] ]) `shouldBe` True
    -- simple tests (same tests as those for the transparent implementation)
    it "ClsTrf 5: knowledge of initial state" $ do
        eval (gossipInit 2) (K "0" (Neg (has 2 1 0))) `shouldBe` True
    it "ClsTrf 6: call shares secrets between agents" $ do
        eval (after 2 [(0,1)]) (Conj [has 2 1 0, has 2 0 1]) `shouldBe` True
    it "ClsTrf 7: call sequence shares secrets between agents" $ do
        eval (after 3 [(0,1),(1,2)]) (has 3 2 0) `shouldBe` True
    it "ClsTrf 8: no faulty experts" $ do
        eval (after 3 [(0,1)]) (Disj [expert 3 i | i <- [0..2]]) `shouldBe` False
    it "ClsTrf 9: all are experts after the correct call sequence" $ do
        isSuccess 3 [(0,1),(1,2),(0,2)] `shouldBe` True

    -- other general higher-order knowledge tests (same tests as those for the transparent implementation)
    it "ClsTrf 10: higher-order knowledge after one call" $ do
        eval (after 3 [(0,1)]) (K "1" (has 3 0 1)) `shouldBe` True
    -- it "simpTrf 11: higher-order knowledge after call sequence" $ do
    --     eval (after 3 [(0,1),(1,2),(2,3),(0,2)]) (K "0" $ Conj [has 3 3 0, K "3" (has 3 2 0)]) `shouldBe` True
\end{code}