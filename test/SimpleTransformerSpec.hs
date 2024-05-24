module SimpleTransformerSpec where

import SimpleTransformer

import Test.Hspec


spec :: Spec
spec = do
    describe "SimpleTransformer" $ do
        it "after same result as individual calls" $ do
            afterSimple 3 [(0,1),(1,2)] `shouldBe` doSimpleCall (doSimpleCall (simpleGossipInit 3) (0,1)) (1,2)
