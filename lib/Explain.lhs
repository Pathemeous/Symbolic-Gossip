\section{Gossip Scene Investigation}\label{sec:Explain}

This section explains the functions that we created to make sense of the current state of a given gossip problem, 
i.e. gossip scene investigation. The functions only work on the unoptimized, Classic Transformer, since the code relies on the 
exact vocabulary being copied. First of all, the code makes use of the following imports:

\begin{code}
module Explain where
   
--import SMCDEL.Examples.GossipS5      <-- fixme: redundant import acc. to vscode?
import SMCDEL.Symbolic.S5
import SMCDEL.Language
import SMCDEL.Other.BDD2Form
import Data.Maybe
\end{code}

One remarkable property of the SMCDEL implementation \cite{GattingerThesis2018} is how the transformer updates the vocabulary 
by copying all of the secret propositions.
This means that in any given transformation, there will be a propositional variable representing a secret 
$S_ij$, as well as a copy of said variable $(S_ij)^o$. Moreover, we have propositions for calls $q_{ij}$. 
In order to prevent overlap between the several groups of variables, a unique value is computed for each propositional variable. 
A propositional variable is of the form $P i$, where $i$ is generated using one of the following functions (\cite{GattingerThesis2018}):

\begin{verbatim}
   -- a has the secret of b
   hasSof :: Int -> Int -> Int -> Prp
   hasSof n a b | a == b    = error "Let's not even talk about that."
                | otherwise = toEnum (n * a + b)

   -- a calls b
   thisCallProp :: (Int,Int) -> Prp
   thisCallProp (i,j) | i < j     = P (100 + 10*i + j)
                      | otherwise = error $ "wrong call: " ++ show (i,j)
\end{verbatim}

In order to make the description of a Knowledge Structure human-readable, we defined the following functions to translate the
encoded propositions: \texttt{prpLibrary} checks whether a proposition denotes a secret, call proposition, or copy of a secret.
The function takes the vocabulary as input, as well as the number of agents, and returns the library from which we can 
decipher propositions in our gossip scene investigation.

\begin{code}
prpLibrary :: [Prp] -> Int -> [(Prp,String)]
prpLibrary prps n = zip prps (prpLibraryHelper prps)
   where 
      -- assign the propositions to secrets, calls, and copies of secrets
      -- and decode each with the appropriate decoder
      prpLibraryHelper :: [Prp]  -> [String]
      prpLibraryHelper [] = []
      prpLibraryHelper prps' = a ++ copyDecoder (drop (div (3*n*(n-1)) 2) prps') a "'"
         where 
            a =    secretDecoder (take (n*(n-1)) prps') 
                ++ callDecoder 0 (take (div (n*(n-1)) 2) (drop (n*(n-1)) prps'))
      
      -- decode secrets
      secretDecoder ::  [Prp] -> [String]
      secretDecoder [] = []
      secretDecoder ((P p):ps)  = ("s"++ show i ++ show j) : secretDecoder ps
         where (i, j) = (p `quot` n, p `rem` n)
      
      -- decode calls
      callDecoder :: Int -> [Prp] -> [String]
      callDecoder k calls | k >= div (n*(n-1)) 2 = []
                          | null calls = []
                          | otherwise = ("q" ++ show i ++ show j) : callDecoder (k + 1) calls
         where 
            (i, j) = getCNums k 0
            getCNums :: Int -> Int -> (Int,Int)
            getCNums k' r'' | (k'+1) < n = (r'',k'+1)
                            | otherwise = getCNums (k'-n+2+r'') (r''+1)
      
      -- decode copies
      copyDecoder :: [Prp] -> [String] -> String -> [String]
      copyDecoder [] _ _ = []
      copyDecoder props lib r = map (++r) lib ++ copyDecoder (drop (length lib) props) lib (r++"'")
\end{code}

Additionally, we wrote the (unsafe) function \texttt{explainPrp}, which takes in a proposition as well as the library, 
to return its meaning (as String). 

\begin{code} 
explainPrp :: Prp -> [(Prp,String)] -> String
explainPrp (P x) prpLib = fromJust (lookup (P x) prpLib)
\end{code}

We follow this up with \texttt{gsi}, our gossip scene investigation, which takes in a knowledge scene and the number of agents, 
and uses \texttt{explainPrp} to make sense of the vocabulary and observations.
\begin{code}
-- Gossip Scene Investigation: GSI. ...like the tv show but with less crime and more gossip. 
-- 
gsi :: KnowScene -> Int -> IO ()
gsi (KnS voc stl obs, s) n = do 
   putStrLn "Vocabulary: "
   mapM_ (putStrLn . (++) " --  " . \p -> explainPrp p lib) voc
   putStrLn "State Law: "
   print (ppFormWith (`explainPrp` lib) (formOf stl))
   putStrLn "Observables: "
   mapM_ (putStrLn . (++) " --  " . (\ x -> fst x ++ ":  " ++ show (map (`explainPrp` lib) (snd x)) )) obs
   putStrLn "Actual state: "
   if null s then putStrLn " --  Nobody knows about any other secret" 
      else 
      mapM_ (putStrLn . (++) " --  " . \p -> explainPrp p lib) s
   where
      lib = prpLibrary voc n
\end{code}

We can then run the following:

\begin{verbatim}
import SMCDEL.Examples.GossipS5
s0 = gossipInit 3
gsi s0 3
s1 = doCall s0 (0,1)
gsi s1 3
\end{verbatim}

which outputs the following  TODO

\begin{verbatim}
Vocabulary: 
 --  s01
 --  s02
 --  s10
 --  s12
 --  s20
 --  s21
State Law: 
"(~s01 & ~s02 & ~s10 & ~s12 & ~s20 & ~s21)"
Observables: 
 --  0:  []
 --  1:  []
 --  2:  []
Actual state: 
 --  Nobody knows about any other secret

Vocabulary: 
 --  s01
 --  s02
 --  s10
 --  s12
 --  s20
 --  s21
 --  q01
 --  q02
 --  q12
 --  s01'
 --  s02'
 --  s10'
 --  s12'
 --  s20'
 --  s21'
State Law: 
"((s01 & ~s02 & s10 & ~s12 & ~s20 & ~s21 & q01 & ~q02 & ~q12 & ~s01' & ~s02' & ~s10' & ~s12' & ~s20' & ~s21') | (~s01 & ((s02 & ~s10 & ~s12 & s20 & ~s21 & ~q01 & q02 & ~q12 & ~s01' & ~s02' & ~s10' & ~s12' & ~s20' & ~s21') | (~s02 & ~s10 & s12 & ~s20 & s21 & ~q01 & ~q02 & q12 & ~s01' & ~s02' & ~s10' & ~s12' & ~s20' & ~s21'))))"
Observables: 
 --  0:  ["q01","q02"]
 --  1:  ["q01","q12"]
 --  2:  ["q02","q12"]
Actual state: 
 --  s01
 --  s10
 --  q01
\end{verbatim}

% \eval{gsi (gossipInit 3) 3}
% \eval{gsi (doCall (gossipInit 3) (0,1)) 3}

%% fixme: latex doesn't understand the eval command, let's change to verbatim. !!

In the future, we hope to also show the law as its BDD (Binary Decision Diagram
\footnote{A Binary Decision Diagram provides a concise representation of a Boolean formula. SMCDEL uses BDDs for the symbolic evaluation 
of logic problems.}) using the tool graphviz. 

% \begin{code}
% -- Here a function that takes a BDD or a form and makes a BDD picture.
% \end{code}

% Taking a higher-level view of Gossip, we can see how from an initial state, there are branches depending on which 
% calls are made, leading to a tree. We write now some code to store these states in a tree. Since an infinite amount of 
% calls can be made, we limit the size of the tree using a \texttt{depth} parameter.

% \begin{code}
% data Tree a = T a [Tree a]
%    deriving(Show)

% -- explainScene :: KnowScene -> Int -> KnowScene
% -- explainScene (KnS voc sLaw obs, s) n = KnS ()

% -- explainGossip :: Int -> Int -> Tree KnowScene
% -- explainGossip n depth = T (gossipInit n) []

% gossipTree :: Int -> Int -> Tree KnowScene
% gossipTree n depth = T (gossipInit n) (gossipBranches (gossipInit n) n depth)  

% gossipBranches :: KnowScene -> Int -> Int -> [Tree KnowScene]
% gossipBranches _ _ 0 = []
% gossipBranches ks n' depth' = [ T (doCall ks (i,j)) (gossipBranches (doCall ks (i,j)) n' (depth'-1)) | i <- gossipers n', j <- gossipers n', i < j ]  
% \end{code}