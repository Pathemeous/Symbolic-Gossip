module ClassicTransformerSpec where


import Test.Hspec hiding ( after )
import SMCDEL.Examples.GossipS5
import SMCDEL.Language
import SMCDEL.Symbolic.S5


spec :: Spec
spec = do
    describe "ClassicTransformer" $ do
        it "clsTrf: higher-order knowledge works" $ do
            eval (after 3 [(0,1),(1,2)]) (K "2" $ has 3 0 1) `shouldBe` True
