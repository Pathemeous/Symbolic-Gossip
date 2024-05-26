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

The observables for agent $i$ - which equal the empty set in the classic implementation - now include the proposition "$S_i j$ for all agents $j$.
Conceptually, these are the propositions that $i$ can observe the truth value of these propositions at any point in the model: factual change does not 
influence the ability of $i$ to observe them. This is only true for propositions involving $i$'s own knowledge. For example, even if Alice can "observe" that 
Bob does not know Charles' secret in the initial model, she cannot know this fact with certainty after a first call has occurred. 

Analogous to the classic implementation, the state \texttt{actual} is initially empty, as it describes all true propositions of the form "$i$ knows the secret of agent $j$".
While agents do know their own secrets, these are not encoded by propositions and therefore not mentioned in the state.

\begin{code}
simpleGossipInit :: Int -> KnowScene
simpleGossipInit n = (KnS vocab law obs, actual) where
    vocab  = [ hasSof n i j | i <- gossipers n, j <- gossipers n, i /= j ]
    law    = boolBddOf Top
    obs    = [ (show i, allSecretsOf n i) | i <- gossipers n ]
    actual = [ ]
\end{code}

The simple transformer is a generall call transformer for any calls.
This allows the transformer to be synchronous (rather than transparent): agents know that a call has taken place, but not necessarily which call.

The event vocabulary $V^+$ contains all fresh variables needed to describe the transformation, just like in the classical transformer.
Contrary to the clasiscal case, $V^+$ not to $V$, which avoids a quick growth of the vocabulary with each call.

The state law $\Theta_-$ (\texttt{changelaws}) is similarly defined as in the classic transformer, allowing the update to compute the factual change $V_-$ and modify the state.

The transformation observables in this transformer are empty, as we will show that the specific update function will only need the observables in the original knowledge structure.

The function \texttt{simpleGossipTransformer} is the simple analogue of the classic transformer \texttt{callTrf} and the transparent variant 
\texttt{callTrfTransparent} from \ref{sec:Transparent}.

\begin{code}
simpleGossipTransformer :: Int -> SimpleTransformerWithFactual
simpleGossipTransformer n = SimTrfWithF eventprops changelaws changeobs where
    -- helper functions to construct the required formulae
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

    -- Change obs are empty as they are not used
    changeobs    = [ (show i, ([],[])) | i <- gossipers n ]
\end{code}
 
The following functions are analogues of those in \texttt{GossipS5.hs} and instead use the simple transformer.

\begin{code}
-- a single call event with a simple transformer
simpleCall :: Int -> (Int,Int) -> StwfEvent
simpleCall n (a,b) = (simpleGossipTransformer n, [thisCallProp (a,b)])

-- execute a simple call event
doSimpleCall :: KnowScene -> (Int,Int) -> KnowScene
doSimpleCall start (a,b) = start `update` simpleCall (length $ agentsOf start) (a,b)

-- execute repeated calls using the simple transformer
afterSimple :: Int -> [(Int, Int)] -> KnowScene
afterSimple n = foldl doSimpleCall (simpleGossipInit n)

-- Some helper functions
allSecretsOf :: Int -> Int -> [Prp]
allSecretsOf n x = [ hasSof n x j | j <- gossipers n, j /= x ]
\end{code}