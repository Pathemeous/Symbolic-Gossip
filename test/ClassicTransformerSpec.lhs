\begin{code}
module ClassicTransformerSpec where


import Test.Hspec hiding ( after )
import SMCDEL.Examples.GossipS5
import SMCDEL.Language
import SMCDEL.Symbolic.S5
\end{code}

We can verify that $K_2 S_01$ is indeed true after the calls $01;12$ in the classic transformer.

\begin{code}
spec :: Spec
spec = do
    describe "ClassicTransformer" $ do
        it "clsTrf: higher-order knowledge works" $ do
            eval (after 3 [(0,1),(1,2)]) (K "2" $ has 3 0 1) `shouldBe` True
\end{code}

Indeed this test passes, highlighting the limitations of our earlier simple transformer.