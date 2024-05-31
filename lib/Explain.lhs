\section{Gossip Scene Investigation}\label{sec:Explain}

This section explains the functions that we created to make sense of the current state of a given gossip problem, 
i.e. gossip scene investigation. The functions only work on the unoptimized, Classic Transformer, since the code relies on the 
exact vocabulary being copied. First of all, the code makes use of the following imports:

\begin{code}
module Explain where
   
import SMCDEL.Symbolic.S5
import SMCDEL.Language
import SMCDEL.Other.BDD2Form
import Data.Maybe

-- import Debug.Trace
\end{code}

One remarkable property of the SMCDEL implementation \cite{GattingerThesis2018} is how the transformer updates the vocabulary 
by copying all of the secret propositions.
This means that in any given transformation, there will be a propositional variable representing a secret 
$S_ij$, as well as a copy of said variable $(S_ij)^o$. Moreover, we have propositions for calls $q_{ij}$. 
In order to prevent overlap between the several groups of variables, a unique value is computed for each propositional variable. 
A propositional variable is of the form $p_i$, where $i$ is generated using one of the following functions (\cite{GattingerThesis2018}):

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

-- decode secrets
secretDecoder ::  [Prp] -> Int -> [String]
secretDecoder [] _ = []
secretDecoder ((P p):ps) n = ("s"++ show i ++ show j) : secretDecoder ps n
      where (i, j) = (p `quot` n, p `rem` n)

prpLibrary :: [Prp] -> Int  -> [(Prp,String)]
prpLibrary prps n = zip prps (prpLibraryHelper prps)
   where 
      -- assign the propositions to secrets, calls, and copies of secrets
      -- and decode each with the appropriate decoder
      prpLibraryHelper :: [Prp]  -> [String]
      prpLibraryHelper [] = []
      prpLibraryHelper prps' = a ++ copyDecoder (drop (div (3*n*(n-1)) 2) prps') a "'"
         where 
            a = secretDecoder (take (n*(n-1)) prps') n ++ callDecoder 0 (take (div (n*(n-1)) 2) (drop (n*(n-1)) prps'))
      
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


prpLibraryTr :: [Prp] -> Int -> [(Int, Int)] -> [(Prp, String)]
prpLibraryTr prps n calls = zip prps (decSec ++ callsNcopies (drop nS prps) calls "'")
   where 
      nS = (n-1)*n
      decSec = secretDecoder (take nS prps) n
      -- decode calls and append a decoded Secrets primed (copies)
      callsNcopies :: [Prp] -> [(Int, Int)] -> String -> [String]
      callsNcopies [] _ _ = []
      callsNcopies _ [] s = map (++s) decSec
      callsNcopies (_:ps) ((a,b):c) s = ["q"++show a++show b++tail s] ++ map (++s) decSec ++ callsNcopies (drop nS ps) c (s++"'")

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
-- Gossip Scene Investigation: GSI... like the TV show but with less crime and more gossip. 
gsiVoc :: KnowScene -> IO()
gsiVoc kns@(KnS voc _ _, _) = do
   putStrLn "Vocabulary: "
   mapM_ (putStrLn . (++) " --  " . \p -> explainPrp p lib) voc
      where
      lib = prpLibrary voc (length $ agentsOf kns)

gsiStLaw :: KnowScene -> IO()
gsiStLaw kns@(KnS voc stl _ ,_) = do 
   putStrLn "State Law: "
   print (ppFormWith (`explainPrp` lib) (formOf stl))
      where
      lib = prpLibrary voc (length $ agentsOf kns) 

gsiObs :: KnowScene -> IO()
gsiObs kns@(KnS voc _ obs ,_) = do 
   putStrLn "Observables: "
   mapM_ (putStrLn . (++) " --  " . (\ x -> fst x ++ ":  " ++ show (map (`explainPrp` lib) (snd x)) )) obs
      where
      lib = prpLibrary voc (length $ agentsOf kns)

gsiState :: KnowScene -> IO()
gsiState kns@(KnS voc _ _,s) = do 
   putStrLn "Actual state: "
   if null s then putStrLn " --  Nobody knows about any other secret" 
      else 
      mapM_ (putStrLn . (++) " --  " . \p -> explainPrp p lib) s
         where
      lib = prpLibrary voc (length $ agentsOf kns)


gsi :: KnowScene -> Maybe [(Int, Int)] -> IO ()
gsi kns@(KnS voc stl obs, s) calls = do
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
      lib | isNothing calls = prpLibrary voc (length $ agentsOf kns)
          | otherwise = prpLibraryTr voc (length $ agentsOf kns) (fromJust calls)
\end{code}

We can then run the following:  % fixme: overfull hbox

\begin{verbatim}
import SMCDEL.Examples.GossipS5
ghci> gsi $ gossipInit 3
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

ghci> gsi $ doCall (gossipInit 3) (0,1)
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
"((s01 & ~s02 & s10 & ~s12 & ~s20 & ~s21 & q01 & ~q02 
      & ~q12 & ~s01' & ~s02' & ~s10' & ~s12' & ~s20' & ~s21') 
   | (~s01 & ((s02 & ~s10 & ~s12 & s20 & ~s21 & ~q01 
      & q02 & ~q12 & ~s01' & ~s02' & ~s10' & ~s12' & ~s20' & ~s21') 
   | (~s02 & ~s10 & s12 & ~s20 & s21 & ~q01 & ~q02 
      & q12 & ~s01' & ~s02' & ~s10' & ~s12' & ~s20' & ~s21'))))"
Observables: 
 --  0:  ["q01","q02"]
 --  1:  ["q01","q12"]
 --  2:  ["q02","q12"]
Actual state: 
 --  s01
 --  s10
 --  q01
\end{verbatim}


In the future, we hope to also show the law as its BDD (Binary Decision Diagram \footnote{A Binary Decision Diagram provides a concise representation of a Boolean formula. SMCDEL uses BDDs for the symbolic evaluation 
of logic problems.}) using the tool \textit{graphviz}. 