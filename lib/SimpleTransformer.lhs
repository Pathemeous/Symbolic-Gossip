\section{Simple Transformer}\label{sec:Simple}

This module describes an implementation of the simple transformer as defined by Daniel Reifsteck in his master's thesis (TODO: cite daniel? how?). 
The simple transformer aims to avoid the exponential blowup of variables that occurs in the classic transformer by copying propositions at each update
and storing the "history" of events in the state law. 
The simple transformer does not change the initial state law throughout the computation. Instead, it directly applies factual change to 
the actual state. 

\begin{code}
module SimpleTransformer where

import HaitianS5
import SMCDEL.Examples.GossipS5
import SMCDEL.Language
import Data.List ((\\))
\end{code}

The model is initialized by the \texttt{simpleGossipInit} function, which is based on the \texttt{gossipInit} function in the GossipS5 file. 
The initial vocabulary contains all propositions of the form "$i$ knows the secret of agent $j$", for all agents $i,j$. 
Whereas the original state law described the situation in which agents only know their own secrets, this definition is too restrictive
for the simple implementation: it prevents the learning of secrets, since the actual state should obey the
state law throughout the computation. Thus, in order not to exclude any possible later states, we chose the law to be simply $\top$.

The observables for agent $i$ - which equal the empty set in the classic implementation - now include the proposition "$i$ knows the secret of $j$"
for each agent $j$. Conceptually, these are the propositions that $i$ can observe the truth value of these propositions at any point in the model: factual change does not 
influence the ability of $i$ to observe them. This is only true for propositions involving $i$ itself; for example, even if Alice can "observe" that 
Bob doesn't know Charles' secret in the initial model, she can't know this fact with certainty after a first call has occurred. 

Analogous to the classic implementation, \texttt{actual} is initially empty, as it describes all true propositions of the form "$i$ knows the secret of agent $j$" (note that 
we don't include encodings of $i$ knowing their own secret in the model). 

\begin{code}
simpleGossipInit :: Int -> KnowScene
simpleGossipInit n = (KnS vocab law obs, actual) where
    vocab  = [ hasSof n i j | i <- gossipers n, j <- gossipers n, i /= j ]
    law    = boolBddOf Top
    obs    = [ (show i, allSecretsOf n i) | i <- gossipers n ]
    actual = [ ]
\end{code}

The simple transformer is defined on a specific call $ab$ (TODO: so it's transparent now?) between agents $a$ and $b$. 
% ??? This automatically makes it transparent, since all agents know by definition which transformer is applied to the model at every step. 
The function \texttt{simpleGossipTransformer} is indeed similar to the transparent transformer \texttt{callTrfTransparent} from \ref{sec:Transparent}.

The new vocabulary contains all fresh variables needed to describe the transformation and are not added to $V$, unlike the classic implementation. 
Since 
  
\begin{code}
simpleGossipTransformer :: Int -> Int -> Int -> SimpleTransformerWithFactual
simpleGossipTransformer n a b = SimTrfWithF eventprops changelaws changeobs where
    thisCallHappens (i,j) = PrpF $ thisCallProp (i,j)
    isInCallForm k = Disj $ [ thisCallHappens (i,k) | i <- gossipers n \\ [k], i < k ]
                        ++ [ thisCallHappens (k,j) | j <- gossipers n \\ [k], k < j ]
    allCalls = [ (i,j) | i <- gossipers n, j <- gossipers n, i < j ]
    -- V+ Event props stay the same as classical transformer
    eventprops = map thisCallProp allCalls
    -- How do we implement the event law? --
    -- Below the law from Classic CallTrf from GossipS5
    --   eventlaw = simplify $
    --     Conj [ Disj (map thisCallHappens allCalls)
    --          -- some call must happen, but never two at the same time:
    --          , Neg $ Disj [ Conj [thisCallHappens c1, thisCallHappens c2]
    --                       | c1 <- allCalls, c2 <- allCalls \\ [c1] ] ]
    -- Theta- Change law stays same as Classic Transformer
    changelaws =
      [(hasSof n i j, boolBddOf $              -- after a call, i has the secret of j iff
          Disj [ has n i j                     -- i already knew j, or
              , Conj (map isInCallForm [i,j]) -- i and j are both in the call or
              , Conj [ isInCallForm i         -- i is in the call and there is some k in
                      , Disj [ Conj [ isInCallForm k, has n k j ] -- the call who knew j
                            | k <- gossipers n \\ [j] ] ]
              ])
      | i <- gossipers n, j <- gossipers n, i /= j ]
      -- set O+ = all the other's secrets for agents a,b and empty for all others
      -- Interleaves the agents a,b to keep the correct ordering of the agents,
      -- which is required for the update-checks (checks agent lists are the same inc order)
    changeobs :: [(Agent, ([Prp], [Prp]))]
    changeobs = [(show k, ([], [])) | k <- gossipers n, k < a ]        ++
                [(show a, (allSecretsOf n b, []))]                     ++
                [(show k, ([], [])) | k <- gossipers n, k > a, k < b ] ++
                [(show b, (allSecretsOf n a, []))]                     ++
                [(show k, ([], [])) | k <- gossipers n, k > b ]

simpleCall :: Int -> (Int,Int) -> StwfEvent
simpleCall n (a,b) = (simpleGossipTransformer n a b, [thisCallProp (a,b)])

doSimpleCall :: KnowScene -> (Int,Int) -> KnowScene
doSimpleCall start (a,b) = start `update` simpleCall (length $ agentsOf start) (a,b)

afterSimple :: Int -> [(Int, Int)] -> KnowScene
afterSimple n = foldl doSimpleCall (simpleGossipInit n)


-- Some helper functions
allSecretsOf :: Int -> Int -> [Prp]
allSecretsOf n x = [ hasSof n x j | j <- gossipers n, j /= x ]
\end{code}