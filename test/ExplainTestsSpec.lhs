\subsection{Gossip Scene Investigation}\label{ssec:ExplainTests}

\begin{code}
module ExplainTestsSpec where

import Explain
import SMCDEL.Examples.GossipS5
import SMCDEL.Language
-- import SMCDEL.Symbolic.S5
-- import Test.QuickCheck

import Test.Hspec hiding (after)

\end{code}

Tests: 

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
           prpLibrary (hasSofs 1) 1 `shouldBe` []
           prpLibrary (hasSofs 2) 2 `shouldBe` [(P 1, "s01"), (P 2, "s10")]
           prpLibrary (hasSofs 5) 5 `shouldBe` zip (hasSofs 5) (enumS 5)
           prpLibrary (hasSofs 10) 10 `shouldBe` zip (hasSofs 10) (enumS 10)
        --it "after calls" $ do 
        --   prpLibrary (callsVoc 3 [(0,1)]) 3 `shouldBe` [] --- How to test this? 
        --it "length" $ do 
           where 
            hasSofs :: Int -> [Prp]
            hasSofs n = [ hasSof n i j | i <- gossipers n, j <- gossipers n, i /= j ]
            enumS :: Int -> [String]
            enumS n = ["s"++ show i ++ show j | i <- gossipers n, j <- gossipers n, i /= j ]
            --callsVoc :: Int -> [(Int, Int)] -> [Prp]
            --callsVoc n sequ = v 
             --   where 
               --     (KnS v _ _, _) = after n sequ

\end{code}