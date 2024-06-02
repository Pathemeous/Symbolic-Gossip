\section{Transparent Transformer}\label{sec:Transparent}

This section describes a variant of the Classic Knowledge Transformer that is implemented for the Transparent Gossip Problem.
This transformer is tailored to the actual call that happens, which makes sure that whenever a call happens, all agents
know this and also know which agents participate.

\hide{
\begin{code}
module Transparent where

import SMCDEL.Examples.GossipS5
import SMCDEL.Language
import SMCDEL.Symbolic.S5
\end{code}
}

We chose to adapt the existing function \texttt{callTrf} defined in \texttt{SMCDEL.GossipS5}, which is the call transformer for the Synchronous Gossip
Problem. Instead of \texttt{Int -> KnowTransformer}, the function is now \texttt{Int -> Int -> Int -> KnowTransformer}, so that
agents $a$ and $b$ are arguments for the transformer for call $(a,b)$.
As in Section \ref{sec:Background}, we define various components of the transformer.

First, the vocabulary $V^+$ (the \texttt{eventprops}) now simply consists of the single proposition for the call between agents $a$ and $b$.
As opposed to the synchronous case, we do not need extra vocabulary to describe all possible calls that could be happening:
all agents know exactly which call happens.

The \texttt{eventlaw}, $\theta^+$ (which originally stated that only one
call happens at a time but not which), is simplified to be the call between agents $a$ and $b$.
The \texttt{changelaws}, $\theta_-$, are quite different from those in the Classic Transformer:
the conditions for the proposition $S_ij$ to be true after \textit{some} call happens
are simplified to the conditions $S_ij$ to be true after the \textit{actual} call $(a,b)$ happens.

For instance, if $i$ is agent $a$, then $i$ knows $j$'s secret after call $ab$ if either
\begin{enumerate}
  \item $i$ knew it already, or
  \item $j$ equals $b$, or
  \item $b$ told $i$ the secret of $j$ during their call.
\end{enumerate}

Finally, the \texttt{eventobs}, $O_i^+$ for each agent $i$, are also simplified to call $(a,b)$,
since there is only one possible event happening and every agent observes it.

\begin{code}
callTrfTransparent :: Int -> Int -> Int -> KnowTransformer
callTrfTransparent n a b = KnTrf eventprops eventlaw changelaws eventobs where
  thisCallHappens = thisCallProp (a,b)
  -- the only event proposition is the current call
  eventprops = [thisCallHappens]

  -- call ab takes place and no other calls happen
  eventlaw = PrpF thisCallHappens

  changelaws =
  -- i has secret of j
      -- case: i is not a and i is not b: then i can not have learned the secret unless it already knew it (has n i j)
    [(hasSof n i j, boolBddOf $ has n i j) | i <- gossipers n, j <- gossipers n, i /= j, i /= a, i /= b] ++
      -- case: i is a, j is not b: then i learned the secret if it already knew it, or b knew the secret of j
    [(hasSof n a j, boolBddOf $ Disj [ has n a j , has n b j ]) | j <- gossipers n, a /= j, b /= j ] ++
      -- case: i is a, j is b: then Top (also: i is b, j is a)
    [(hasSof n a b, boolBddOf Top)] ++ [(hasSof n b a, boolBddOf Top)] ++
      -- case i is b, j is not a: synonymous to above
    [(hasSof n b j, boolBddOf $ Disj [ has n b j , has n a j ]) | j <- gossipers n, a /= j, b /= j ]

  eventobs = [(show k, [thisCallHappens]) | k <- gossipers n]
\end{code}

Since the transparent transformer has the same type as the synchronous variant, we inherited its update function from the SMCDEL library.
The following functions were adapted from the original implementation to perform the transparent update:

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