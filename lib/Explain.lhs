\section{Gossip Scene Investigation}\label{sec:Explain}

This section explains functions that we created to make sense of the current state of a given gossip problem, i.e. gossip scene investigation. We begin with the following imports.

\begin{code}
module Explain where
   
import SMCDEL.Examples.GossipS5
import SMCDEL.Symbolic.S5
import SMCDEL.Language
import SMCDEL.Other.BDD2Form
import Data.Maybe
\end{code}

One of the differences between SMCDEL and \cite{GattingerThesis2018} is how the transformer updates the vocabulary by copying all of the secret propositions. This means in any given transformation, there will be a propositional variable representating a secret, as well as a copy of said variable. 

%% fixme: add an explanation of why the secrets are copied 

The first thing we did was beginning by writing \texttt{prpLibrary} to decode propositions into whether they were secrets, call propositions, or copies of secrets. The function works by taking in the vocabulary, as well as the number of agents, and returns the library. We also write an (unsafe) function \texttt{explainPrp}, which takes in a proposition as well as the library, to return its meaning. 

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
         
explainPrp :: Prp -> [(Prp,String)] -> String
explainPrp (P x) prpLib = fromJust (lookup (P x) prpLib)
\end{code}

We follow this up with \texttt{gsi}, our gossip scene investigation, which takes in a state, the number of agents, and uses \texttt{explainPrp} to make sense of the vocabulary and observations. Further work must be done to make sense of the state law. 
\begin{code}
-- Gossip Scene Investigation: GSI. ...like the tv show but with less crime nd more gossip. 
gsi :: KnowScene -> Int -> IO ()
gsi (KnS voc stl obs, s) n = do 
   putStrLn "Vocabulary: "
   mapM_ (putStrLn . (++) " --  " . \p -> explainPrp p lib) voc
   putStrLn "State Law: "
   -- Translate BDDS back to formula. 
   -- Can be made a bit nicer, maybe make it latex or smth. 
   -- Still needs the propositions in the formula translated/explained
   print (formOf stl)
   putStrLn "Observables: "
   mapM_ (putStrLn . (++) " --  " . (\ x -> fst x ++ ":  " ++ show (map (`explainPrp` lib) (snd x)) )) obs
   putStrLn "Actual state: "
   if null s then putStrLn " --  Nothing is true" 
      else 
      mapM_ (putStrLn . (++) " --  " . \p -> explainPrp p lib) s
   where
      lib = prpLibrary voc n
\end{code}

%% fixme: demonstate the output of using gsi?? 

In the future, we hope to also show the law as its BDD using the tool graphviz. 

\begin{code}
-- Here a function that takes a BDD or a form and makes a BDD picture.
\end{code}

Taking a higher-level view of Gossip, we can see how from an initial state, there are branches depending on which calls are made, leading to a tree. We write now some code to store these states in a tree. Since an infinite amount of calls can be made, we limit the size of the tree using a \texttt{depth} parameter.

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