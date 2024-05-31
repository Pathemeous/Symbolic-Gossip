\subsection{Simple Transformer}\label{sec:Simple}

\hide{
module SimpleTransformer where

import SmpTrfS5
import SMCDEL.Symbolic.S5
import SMCDEL.Examples.GossipS5
import SMCDEL.Language
import Data.List ((\\))
\end{code}
}

As we have seen, the (classical) implementation of transformer inserts propositions into the vocabulary and modfies the state law,
which is suspected to be the mani source of computation.
We therefore consider a different notion of transformers called \emph{Simple Transformers}, which was introduced by \cite{danielMasterThesis}.
There is a notion of such transformers with and without factual change. For gossip we will only use the simple transformers with factual change.

The simple transformer is a less expressive transformer that is `simple' in the sense that it disregards complexities in the knowledge semantics.
The benefit of this simplification is that it does not modify the vocabulary or state law,
which are precisely the parts of the knowledge structure that grow uncontrollably in the classical case.

The transformer still uses the event propositions in $V^+$ and the change laws $\theta_-$ to determine the factual change $V_-$,
but instead uses that result to modify the state rather than the knowledge structure.

Meanwhile, observables can be mutated similarly to the classical case, with the addition of the notion to remove observables from agents too.

In our implementation of the Simple Transformer for gossip, we will not use the observable management and instead only rely on the transformer to compute the factual change.

\subsubsection{Simple Initial Knowledge Scene}
Due to the limitations in changing the knowledge structure with every update, we must make minimal changes to the initial knowledge scene.

The model is initialized by the \texttt{gossipInitSimple} function,
which is a modification of the \texttt{gossipInit} function part of the Gossip implemenation in SMCDEL \texttt{GossipS5} file.

\begin{code}
-- Initialize a gossip scene for the simple transformer
gossipInitSimple :: Int -> KnowScene
gossipInitSimple n = (KnS vocab law obs, actual) where
    vocab  = [ hasSof n i j | i <- gossipers n, j <- gossipers n, i /= j ]
    law    = boolBddOf Top                                                  -- Top
    obs    = [ (show i, allSecretsOf n i) | i <- gossipers n ]              -- observe all own secret knowledge
    actual = [ ]

-- Retrieve for some agent x all secret atoms of the form $S_xi$
allSecretsOf :: Int -> Int -> [Prp]
allSecretsOf n x = [ hasSof n x j | j <- gossipers n, j /= x ]
\end{code}

The vocabulary \texttt{vocab} stays the same and contains all secret atoms from the language, the state law and observables however are modified.

Whereas the state law in \texttt{gossipInit} describes the situation in which agents only know their own secrets, this definition is too restrictive
for the simple implementation: it prevents the learning of secrets.
In order not to exclude any possible later states, we chose a simple state law of $\theta = \top$.

The observables \texttt{obs} are slightly different too.
While in the classical case these were empty, in the simple transformer we want them to reflect each agent's own secret-knowledge atoms.
That is, each agent $x$ can observe the set $\{S_xj \mid j \in Ag \land x \neq j\}$ where $Ag$ is the set of all agents.

Conceptually, these observables make sense to be true from the very start: an agent should be aware of what secrets they know themselves.

Analogous to the classic implementation,
the state \texttt{actual} is initially empty as it describes all true propositions of the form "$i$ knows the secret of agent $j$".
Note specifically that we again ignore the atoms $S_aa$: while agents do know their own secrets,
these are not encoded by propositions and therefore not mentioned in the state.

\subsubsection{The Simple Transformer for Gossip}
We wil now define the transformer itself.
The function \texttt{CallTrfSimple} is the simple analogue of the classic transformer \texttt{callTrf} from \cite{GattingerThesis2018}.
Note that we have a single transformer to execute any of the calls,
in order for the semantics to be synchronous.

The event vocabulary $V^+$ contains again all fresh variables needed to describe the transformation, just like in the classical transformer.

The state law $\theta_-$ (\texttt{changelaws}) is similarly defined as in the classic transformer,
allowing the update to compute the factual change $V_-$ and modify the state

The transformation observables in this transformer are empty,
as we will show that the specific update function will only need the observables in the original knowledge structure.

\begin{code}
callTrfSimple :: Int -> SimpleTransformerWithFactual
callTrfSimple n = SimTrfWithF eventprops changelaws changeobs where
    -- helper functions to construct the required formulae
    thisCallHappens (i,j) = PrpF $ thisCallProp (i,j)
    isInCallForm k = Disj $ [ thisCallHappens (i,k) | i <- gossipers n \\ [k], i < k ]
                        ++ [ thisCallHappens (k,j) | j <- gossipers n \\ [k], k < j ]
    allCalls = [ (i,j) | i <- gossipers n, j <- gossipers n, i < j ]

    -- V+ event props stay the same as classic transformer
    eventprops = map thisCallProp allCalls

    -- theta- change law stays same as classic transformer
    changelaws =
      [(hasSof n i j, boolBddOf $              -- after a call, i has the secret of j iff
          Disj [ has n i j                     -- i already knew j, or
              , Conj (map isInCallForm [i,j]) -- i and j are both in the call or
              , Conj [ isInCallForm i         -- i is in the call and there is some k in
                      , Disj [ Conj [ isInCallForm k, has n k j ] -- the call who knew j
                            | k <- gossipers n \\ [j] ] ]
              ])
      | i <- gossipers n, j <- gossipers n, i /= j ]

    -- Change observables are empty as they are not used
    changeobs    = [ (show i, ([],[])) | i <- gossipers n ]
\end{code}

The following functions are analogues of those in originally defined in SMCDEL \texttt{GossipS5.hs} and instead use the simple transformer.

\begin{code}
-- construct a a single call event with a simple transformer
simpleCall :: Int -> (Int,Int) -> StwfEvent
simpleCall n (a,b) = (callTrfSimple n, [thisCallProp (a,b)])

-- execute a simple call event
doCallSimple :: KnowScene -> (Int,Int) -> KnowScene
doCallSimple start (a,b) = start `update` simpleCall (length $ agentsOf start) (a,b)

-- execute repeated calls using the simple transformer
afterSimple :: Int -> [(Int, Int)] -> KnowScene
afterSimple n = foldl doCallSimple (gossipInitSimple n)

-- evaluate if the sequence cs is successful for n agents
isSuccessSimple :: Int -> [(Int,Int)] -> Bool
isSuccessSimple n cs = evalViaBdd (afterSimple n cs) (allExperts n)
\end{code}
