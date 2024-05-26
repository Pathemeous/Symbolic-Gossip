\section{Simple Transformer}\label{sec:Simple}

%% fixme: If you can connect this to Background.tex a bit more by using the symbols 
%% (V^+, \Theta^+,\Theta_-,O_k^+) I think it would help with the flow of the paper by 
%% letting the reader see how these things change. I would do it as I did it in Transparent.lhs 
%% but I don't want to mess it up since I'm not as familiar with SimpleTransformer

This module describes an implementation of the simple transformer as defined by Daniel Reifsteck in his master's thesis \cite{danielMasterThesis} (Note: this Thesis is not publically available). 
The simple transformer aims to avoid the exponential blowup of variables that occurs in the classic transformer by copying propositions at each update
and storing the "history" of events in the state law. 
The simple transformer does not change the initial state law throughout the computation. Instead, it directly applies factual change to 
the actual state. 

%% For example, perhaps we could add above, "As we have seen, the even vocabulary $V^+$ instantiates new 
%% variables which remain as copies in the model, as well as updating the law $\Theta$ with huge formulas
%% $\Theta^+$ and $\Theta_-$ encoding the factual change." I'm not sure this is correct, but it would 
%% connect things with what we have done way more :D


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

The simple transformer is defined on a specific call $ab$ between agents $a$ and $b$. 
% ??? This automatically makes it transparent, since all agents know by definition which transformer is applied to the model at every step? 
Technically all agents know which transformer is applied to the model at every step, which raises the question if this implementation of the 
simple transformer doesn't induce a transparent update. However, the definition of the new observables (\texttt{changeobs}) prevents this from happening. 
The observables in this implementation actually force the transformer to be defined on a specific call, as otherwise we can't store which agents learn 
new secrets after a specific update has happened.
The function \texttt{simpleGossipTransformer} is the simple analogue of the classic transformer \texttt{callTrf} and the transparent variant 
\texttt{callTrfTransparent} from \ref{sec:Transparent}.

The new vocabulary contains all fresh variables needed to describe the transformation and are not added to $V$, unlike the classic implementation. 
Since the agents don't know which call actually happens, the vocabulary describes all possible calls. Note the absence of the event law: it's not part of 
the transformer as it's fixed throughout the model. The \texttt{changelaws} are defined as in the classic transformer, since the preconditions for agent $a$
to know agent $b$'s secret haven't changed. 


  
\begin{code}
simpleGossipTransformer :: Int -> Int -> Int -> SimpleTransformerWithFactual
simpleGossipTransformer n a b = SimTrfWithF eventprops changelaws changeobs where
    thisCallHappens (i,j) = PrpF $ thisCallProp (i,j)
    isInCallForm k = Disj $ [ thisCallHappens (i,k) | i <- gossipers n \\ [k], i < k ]
                        ++ [ thisCallHappens (k,j) | j <- gossipers n \\ [k], k < j ]
    allCalls = [ (i,j) | i <- gossipers n, j <- gossipers n, i < j ]
    
    -- V+ event props stay the same as classic transformer
    eventprops = map thisCallProp allCalls
    
    -- Theta- change law stays same as classic transformer
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
\end{code}

\begin{code}
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