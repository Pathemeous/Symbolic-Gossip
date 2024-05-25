\section{Function to explain Gossip scenes, i.e. Gossip Scene Investigation. }\label{explain}

This section explains some functions that we use to make sense of knowledge scenes of gossip. 
(Works for transformers that copy the entire vocab)

\begin{code}
module Explain where
   
import SMCDEL.Examples.GossipS5
import SMCDEL.Symbolic.S5
import SMCDEL.Language
import SMCDEL.Other.BDD2Form
import Test.QuickCheck
import Data.Maybe

import Debug.Trace

\end{code}




\begin{code}

prpLibrary :: [Prp] -> Int -> [(Prp,String)]
prpLibrary prps n = zip prps (prpLibraryHelper prps "")
   where 
      prpLibraryHelper :: [Prp]  -> String -> [String]
      prpLibraryHelper [] _ = []
      prpLibraryHelper prps' r = let 
                                    a = secretDecoder 0 (take (n*(n-1)) prps') r ++ callDecoder 0 (take (div (n*(n-1)) 2) (drop (n*(n-1)) prps')) r 
                                       in 
                                       a ++ copyDecoder (drop (div (3*n*(n-1)) 2) prps') a "'"
      secretDecoder :: Int -> [Prp] -> String -> [String]
      secretDecoder _ [] _ = []
      secretDecoder k ((P p):ps) r' |  k >= n*(n-1) + n = []
                                    |  otherwise = ("S_{"++ show i ++ "}"++ show j ++ r') : secretDecoder (k+1) ps r'
         where (i, j) = (p `quot` n, p `rem` n)
      callDecoder :: Int -> [Prp] -> String -> [String]
      callDecoder k calls r' | k >= div (n*(n-1)) 2 = []
                             | null calls = []
                             | otherwise = ("q" ++ show i ++ show j ++ r') : callDecoder (k + 1) calls r'
         where 
            (i, j) = getCNums k 0
            getCNums :: Int -> Int -> (Int,Int)
            getCNums k' r'' | (k'+1) < n = (r'',k'+1)
                            | otherwise = getCNums (k'-n+2+r'') (r''+1)
      copyDecoder :: [Prp] -> [String] -> String -> [String]
      copyDecoder [] _ _ = []
      copyDecoder props lib r = map (++r) lib ++ copyDecoder (drop (length lib) props) lib (r++"'")
         
explainPrp' :: Prp -> [(Prp,String)] -> String
explainPrp' (P x) prpLib = fromJust (lookup (P x) prpLib)

-- Gossip Scene Investigation: GSI. ...like the tv show but with less crime nd more gossip. 
gsi :: KnowScene -> Int -> IO ()
gsi (KnS voc stl obs, s) n = do 
   putStrLn "vocab: "
   mapM_ (putStrLn . (++) " --  " . \p -> explainPrp' p lib) voc
   putStrLn "State Law: "
   -- Translate BDS back to formula. 
   -- Can be made a bit nicer, maybe make it latex or smth. 
   -- Still needs the propositions in the formula translated/explained
   print (formOf stl)
   putStrLn "Observables: "
   mapM_ (putStrLn . (++) " --  " . (\ x -> fst x ++ ":  " ++ show (map (`explainPrp'` lib) (snd x)) )) obs
   putStrLn "actual state: "
   if null s then putStrLn " --  Nothing is true" 
      else 
      mapM_ (putStrLn . (++) " --  " . \p -> explainPrp' p lib) s
   where
      lib = prpLibrary voc n

\end{code}





\begin{code}

data Tree a = T a [Tree a]
   deriving(Show)

-- explainScene :: KnowScene -> Int -> KnowScene
-- explainScene (KnS voc sLaw obs, s) n = KnS ()

-- explainGossip :: Int -> Int -> Tree KnowScene
-- explainGossip n depth = T (gossipInit n) []

gossipTree :: Int -> Int -> Tree KnowScene
gossipTree n depth = T (gossipInit n) (gossipBranches (gossipInit n) n depth)  

gossipBranches :: KnowScene -> Int -> Int -> [Tree KnowScene]
gossipBranches _ _ 0 = []
gossipBranches ks n' depth' = [ T (doCall ks (i,j)) (gossipBranches (doCall ks (i,j)) n' (depth'-1)) | i <- gossipers n', j <- gossipers n', i < j ]  

\end{code}