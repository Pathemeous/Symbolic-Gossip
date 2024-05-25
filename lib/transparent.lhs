
\section{Transparent Transformer}\label{sec:Transparent}

This module describes a transparent (also: observable) implementation of the classic transformer (TODO: cite malvin?). 
The transformer is tailored to the actual call that happens, which makes sure that 
whenever a call happens, all agents know this and also know which agents participate. 

The module imports the GossipS5 and Symbolic.S5 modules from SMCDEL, which define the synchronous classic transformer,
and the Language module. 
 
\begin{code}
module Transparent where

import SMCDEL.Examples.GossipS5
import SMCDEL.Language
import SMCDEL.Symbolic.S5
import Data.List
\end{code}

Because the transparent transformer is a variant of the synchronous transformer, we chose to 
adapt the existing function \texttt{callTrf} from GossipS5. Instead of \texttt{Int -> KnowTransformer}, the function 
is now \texttt{Int -> Int -> Int -> KnowTransformer}, so that agents $a$ and $b$ are arguments for the transformer 
for call ab. 

\texttt{isInCallForm}, which describes the conditions for agent $k$ to be in a call, is now not a 
disjunction of possible calls, but requires $k$ to be either $a$ or $b$. \textittt{thisCallHappens} is only defined
for the agents performing the actual call. The \texttt{eventlaw} (which originally stated that only one 
call happens at a time) is simplified to describe that only call ab happens. The \texttt{changelaws} are 
identical to those of the synchronous variant. 
The \texttt{eventobs} (a list of tuples describing which events each agent can observe) is also simplified to call ab.

\begin{code}
callTrfTransparent :: Int -> Int -> Int -> KnowTransformer
callTrfTransparent n a b = KnTrf eventprops eventlaw changelaws eventobs where
  -- agent k is in the call ab if a calls k (so k==b) or k calls b (so k==a)
  isInCallForm k | k == a = Top 
                 | k == b = Top 
                 | otherwise = Bot
                 
  thisCallHappens = thisCallProp (a,b)
  eventprops = [thisCallHappens]

  -- call ab takes place and no other calls happen
  eventlaw = Conj [PrpF thisCallHappens,
                 Conj [Neg (PrpF $ thisCallProp (i,j)) | i <- gossipers n
                                                       , j <- gossipers n
                                                       , not ((i == a && j == b) || (i == b && j == a))
                                                       , i < j ]]
  changelaws =
    [(hasSof n i j, boolBddOf $             
        Disj [ has n i j                     
             , Conj (map isInCallForm [i,j]) 
             , Conj [ isInCallForm i         
                    , Disj [ Conj [ isInCallForm k, has n k j ] 
                           | k <- gossipers n \\ [j], a<k && k<b ]]
             ])
    | i <- gossipers n, j <- gossipers n, i /= j ]

  eventobs = [(show k, [thisCallHappens]) | k <- gossipers n]
\end{code}

Since the transparent transformer has the same type as the synchronous variant, we inherited its update function. 
The following functions were adapted to perform the transparent update:

\begin{code}
callTransparent :: Int -> (Int,Int) -> Event
callTransparent n (a,b) = (callTrfTransparent n a b, [thisCallProp (a,b)])

doCallTransparent :: KnowScene -> (Int,Int) -> KnowScene
doCallTransparent start (a,b) = start `update` callTransparent (length $ agentsOf start) (a,b)

afterTransparent :: Int -> [(Int,Int)] -> KnowScene
afterTransparent n = foldl doCallTransparent (gossipInit n)

isSuccessTransparent :: Int -> [(Int,Int)] -> Bool
isSuccessTransparent n cs = evalViaBdd (afterTransparent n cs) (allExperts n)
\end{code}
