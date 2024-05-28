\section{Transparent Transformer}\label{sec:Transparent}

This section describes how we wrote an implementation of the classic Knowledge Transformer for the Transparent Gossip Problem. This transformer is tailored to the actual call that happens, which makes sure that whenever a call happens, all agents know this and also know which agents participate. 

We begin by importing the \texttt{GossipS5} and \texttt{Symbolic.S5} modules from SMCDEL, which define the synchronous classic transformer, and the \texttt{Language} module. 
 
\begin{code}
module Transparent where

import SMCDEL.Examples.GossipS5
import SMCDEL.Language
import SMCDEL.Symbolic.S5
import Data.List
\end{code}

We chose to adapt the existing function \texttt{callTrf} from GossipS5, which is the call transformer for the Synchronous Gossip Problem. Instead of \texttt{Int -> KnowTransformer}, the function is now \texttt{Int -> Int -> Int -> KnowTransformer}, so that agents $a$ and $b$ are arguments for the transformer for call ab. 

As in Section \ref{sec:Background}, we redefine how to update the vocabulary, law, and observations of each agent. 

First, the vocabulary $V^+$, called \texttt{thisCallHappens}, is now simply the call between agents $a$ and $b$; as opposed to the synchronous case, we do not need to add any other new call variables, as all agents know exactly which call happens.

We define a helper function \texttt{isInCallForm}, which describes the conditions for agent $k$ to be in a call, and is now not a disjunction of possible calls as in the synchronous case, but requires $k$ to be either $a$ or $b$. \texttt{thisCallHappens} is only defined for the agents performing the actual call. 

The \texttt{eventlaw} $\theta^+$ (which originally stated that only one 
call happens at a time) is simplified to describe that only one call between $a$ and $b$ happens. Moreover, \texttt{changelaws} $\theta^-$ are identical to those of the synchronous variant. The \texttt{eventobs} $O_k^+$ are also simplified to the call between $a$ and $b$, as every agent observes the call.

\begin{code}
callTrfTransparent :: Int -> Int -> Int -> KnowTransformer
callTrfTransparent n a b = KnTrf eventprops eventlaw changelaws eventobs where
  -- agent k is in the call ab if a calls k (so k==b) or k calls b (so k==a)
  isInCallForm k | k == a = Top 
                 | k == b = Top 
                 | otherwise = Bot
                 
  thisCallHappens = thisCallProp (a,b)
  -- * eventprops = [thisCallHappens]
  eventprops = []  -- Malvin claims that this can be empty

  -- call ab takes place and no other calls happen
  eventlaw = Conj [PrpF thisCallHappens,
                 Conj [Neg (PrpF $ thisCallProp (i,j)) | i <- gossipers n
                                                       , j <- gossipers n
                                                       , not ((i == a && j == b) || (i == b && j == a))
                                                       , i < j ]]
  changelaws =
  -- i has secret of j 
      -- case: i is not a or b: then i can not have learned the secret unless it already knew it (has n i j)
    [(hasSof n i j, boolBddOf $ has n i j) | i <- gossipers n, j <- gossipers n, i /= j, i /= a || i /= b] ++
      -- case: i is a, j is not b: then i learned the secret if it already knew it, or b knew the secret of j
    [(hasSof n a j, boolBddOf $ Disj [ has n a j , has n b j ]) | j <- gossipers n, a /= j ] ++
      -- case: i is a, j is b: then Top (also: i is b, j is a)
    [(hasSof n a b, boolBddOf Top)] ++ [(hasSof n b a, boolBddOf Top)] ++ 
      -- case i is b, j is not a: synonymous to above
    [(hasSof n b j, boolBddOf $ Disj [ has n b j , has n a j ]) | j <- gossipers n, b /= j ]

--   *** changelaws =
--    [(hasSof n i j, boolBddOf $             
--        Disj [ has n i j                     
--             , Conj (map isInCallForm [i,j]) 
--             , Conj [ isInCallForm i         
--                    , Disj [ Conj [ isInCallForm k, has n k j ] 
--                           | k <- gossipers n \\ [j], a<k && k<b ]]
--             ])
--    | i <- gossipers n, j <- gossipers n, i /= j ]


  eventobs = [(show k, [thisCallHappens]) | k <- gossipers n]
\end{code}

Since the transparent transformer has the same type as the synchronous variant, we inherited its update function. The following functions were adapted from the original implementation to perform the transparent update:

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
