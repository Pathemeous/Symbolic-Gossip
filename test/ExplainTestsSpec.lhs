\subsection{Gossip Scene Investigation}\label{ssec:ExplainTests}

\begin{code}
module ExplainTestsSpec where

import Explain
import SMCDEL.Examples.GossipS5
import SMCDEL.Language
import SMCDEL.Symbolic.S5
-- import Test.QuickCheck
import Transparent

import Test.Hspec hiding (after)

\end{code}

\begin{code}
spec :: Spec
spec = do 
    describe "secret translation: " $ do 
        it "single secrets " $ do 
            secretDecoder [P (2*0 + 1)] 2 `shouldBe` ["s01"]
            secretDecoder [P (4*2 + 3)] 4 `shouldBe` ["s23"]
            secretDecoder [P (100*50 + 53)] 100 `shouldBe` ["s5053"]
        it "init " $ do 
            prpLibrary (hasSofs 1) 1 `shouldBe` []
            prpLibrary (hasSofs 2) 2 `shouldBe` [(P 1, "s01"), (P 2, "s10")]
            prpLibrary (hasSofs 5) 5 `shouldBe` zip (hasSofs 5) (enumS 5)
            prpLibrary (hasSofs 10) 10 `shouldBe` zip (hasSofs 10) (enumS 10)
            prpLibraryTr (hasSofs 1) 1 [] `shouldBe` []
            prpLibraryTr (hasSofs 2) 2 [] `shouldBe` [(P 1, "s01"), (P 2, "s10")]
            prpLibraryTr (hasSofs 5) 5 [] `shouldBe` zip (hasSofs 5) (enumS 5)
            prpLibraryTr (hasSofs 10) 10 [] `shouldBe` zip (hasSofs 10) (enumS 10)
        it "synchronous calls: " $ do 
            length (prpLibrary (callsVoc 3 [(0,1)]) 3) `shouldBe` 15
            length (prpLibrary (callsVoc 3 [(0,1), (1,2)]) 3) `shouldBe` 24
            length (prpLibrary (callsVoc 10 [(0,1), (1,2), (2,5), (7,8), (1,9)]) 10) `shouldBe` 765
        it "transparent calls: " $ do 
            length (prpLibraryTr (callsVoc' 3 [(0,1)]) 3 [(0,1)]) `shouldBe` 13
            length (prpLibraryTr (callsVoc' 3 [(0,1), (1,2)]) 3 [(0,1), (1,2)]) `shouldBe` 20
            length (prpLibraryTr (callsVoc' 10 [(0,1), (1,2), (2,5), (7,8), (1,9)]) 10 [(0,1), (1,2), (2,5), (7,8), (1,9)]) `shouldBe` 545
        -- For gsi, I haven't got a clue how to test IO () functions. 
           where 
            hasSofs :: Int -> [Prp]
            hasSofs n = [ hasSof n i j | i <- gossipers n, j <- gossipers n, i /= j ]
            enumS :: Int -> [String]
            enumS n = ["s"++ show i ++ show j | i <- gossipers n, j <- gossipers n, i /= j ]
            callsVoc :: Int -> [(Int, Int)] -> [Prp]
            callsVoc n sequ = v 
                where 
                    (KnS v _ _, _) = after n sequ
            callsVoc' :: Int -> [(Int, Int)] -> [Prp]
            callsVoc' n sequ = v 
                where 
                    (KnS v _ _, _) = afterTransparent n sequ

\end{code}