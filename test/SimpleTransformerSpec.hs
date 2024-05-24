module SimpleTransformerSpec where

import SimpleTransformer

import Test.Hspec hiding ( after )
import SMCDEL.Examples.GossipS5
import SMCDEL.Language
import HaitianS5


spec :: Spec
spec = do
    describe "SimpleTransformer" $ do
        it "after same result as individual calls" $ do
            afterSimple 3 [(0,1),(1,2)] `shouldBe` doSimpleCall (doSimpleCall (simpleGossipInit 3) (0,1)) (1,2)
        it "secret was exchanged after 1 call" $ do
            eval (afterSimple 3 [(0,1),(1,2)]) (K "0" $ has 3 0 1) `shouldBe` True
        it "secret learnt in first was exchanged in second call" $ do
            eval (afterSimple 3 [(0,1),(1,2)]) (K "2" $ has 3 2 0) `shouldBe` True
        it "SmpTrf: higher-order knowledge fails" $ do
            eval (afterSimple 3 [(0,1),(1,2)]) (K "2" $ has 3 0 1) `shouldBe` False
