\begin{code}
module ExplainTestsSpec where

import Explain
import SMCDEL.Examples.GossipS5
import SMCDEL.Language
import Test.QuickCheck

import Test.Hspec

\end{code}

Tests: \\
\begin{itemize}
    \item secret propositions are translated correctly 
    \item the vocabulary has correct length 
    \item after a call the state is updated correct
\end{itemize}

\begin{code}
spec :: Spec
spec = do 
    describe "secret translation:" $ do 
        it "init " $ do 
           prpLibrary ([ hasSof 2 i j | i <- gossipers 2, j <- gossipers 2, i /= j ]) 2 `shouldBe` [(P 1, "S_{0}1"), (P 2, "S_{1}0")]
           --prpLibrary ([ hasSof 3 i j | i <- gossipers 3, j <- gossipers 3, i /= j ]) 3 `shouldBe` [(P 1, "S_{0}1"), (P 2, "S_{1}0")]

        --it "after call" $ do 

\end{code}