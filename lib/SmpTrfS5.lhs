\subsubsection{Updates using the Simple Transformer}

While the original definition of the Simple Transformer in \cite{danielMasterThesis} specifies how the new knowledge scene is constructed,
we have to modify it for our Gossip-specific observable management to work as desired.

The following code extends the SMCDEL library,
specifically the S5-specific symbolic implementation \texttt{SMCDEL.Symbolic.S5} with Simple Transformers with Factual Change.

We limit ourselves to the implementation of only the pointed update.
Additionally, we do not extend the newly defined structures for existing SMCDEL functions such as \texttt{eval} or \texttt{bddOf},
as these extensions are not necessary for our case and should instead be made on the SMCDEL repository directly.

\begin{code}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module SmpTrfS5 where

{-
- This file is a partial copy Symbolic.S5 of Haitian's fork of SMCDEL
  the file includes definitions for Simple Transformers (SmpTrf).
- The update function for SmpTrf with factual change is specific to Gossip
- NOTE: Due other changes in Haitian's fork and the SMCDEL main repo,
  dynamic operators in formulae do not work.
  Instead update the knowledge structure
-}
\end{code}

\hide{
\begin{code}
import Data.HasCacBDD hiding (Top,Bot)
import Data.List

import SMCDEL.Internal.Help
import SMCDEL.Language
import SMCDEL.Symbolic.S5
\end{code}
}

We first define the new datatype for the simple transformer.
The following definitions were written by \cite{HaitianHanabi}.

\begin{code}
{-
    Simple transformer with factual change
-}
data SimpleTransformerWithFactual = SimTrfWithF
  [Prp]                     -- V+ is a set of new variables encoding of a set of events
  [(Prp,Bdd)]               -- Theta- assigns a formula to each modified variable.
  [(Agent,([Prp],[Prp]))]   -- O+ and O- for each agent
  deriving (Eq,Show)

instance Pointed SimpleTransformerWithFactual State
type StwfEvent = (SimpleTransformerWithFactual,State)
\end{code}

While \cite{HaitianHanabi} also implemented the update function as defined by \cite{danielMasterThesis},
we here modify it to instead work specifically for our case of a Gossip transformation.

\begin{code}
-- The following instance is modified from Haitian's implementation of the
-- general simple transformer definition
-- It is *only* applicable to synchronous Gossip calls
instance Update KnowScene StwfEvent where
  checks = [haveSameAgents]
  unsafeUpdate kns@(KnS v th obs,s) (SimTrfWithF _ thetaminus trfObs, x) = (newkns, newstate) where
    -- gossip helper functions to be able to find the current two agents in the call
    thisCallProp :: (Int,Int) -> Prp
    thisCallProp (i,j) | i < j     = P (100 + 10*i + j)
                      | otherwise = error $ "wrong call: " ++ show (i,j)

    n = length $ agentsOf kns
    gossipers :: [Int]
    gossipers = [0..(n-1)]

    allCalls = [ (i,j) | i <- gossipers, j <- gossipers, i < j ]
    allCallprops = map thisCallProp allCalls
    callPropResolve = zip allCallprops allCalls

    -- the transformation state x contains only 1 call proposition
    inThisCall :: (Int, Int)
    inThisCall = callPropResolve ! head x

    -- Compute special observable management for Gossip
    -- Calling agents get their own original observables O_i plus the secrets that they other know
    -- which we define by the intersection of the other agent's observables with the state
    newobs = [ (show i,obs ! show i) | i <- gossipers, i < fst inThisCall ] ++
             [ (show (fst inThisCall) , callerObs ++ intersect newstate calleeTrfObs) ] ++             -- caller
             [ (show i,obs ! show i) | i <- gossipers, i > fst inThisCall, i < snd inThisCall ] ++
             [ (show (snd inThisCall) , calleeObs ++ intersect newstate callerTrfObs) ] ++             -- callee
             [ (show i,obs ! show i) | i <- gossipers, i > snd inThisCall  ] where
                 -- determine the O and O^+ a for agents a,b that are calling (caller,callee)
                callerObs :: [Prp]
                callerObs = obs ! show ( fst inThisCall)
                calleeObs :: [Prp]
                calleeObs = obs ! show ( snd inThisCall)
                callerTrfObs :: [Prp]
                callerTrfObs = fst $ trfObs ! show ( fst inThisCall)
                calleeTrfObs :: [Prp]
                calleeTrfObs = fst $ trfObs ! show ( snd inThisCall)

    newkns = KnS v th newobs -- keep V and Theta but changes obs
    newstate = sort ((s \\ map fst thetaminus) ++ filter (\ p -> bddEval (s ++ x) (thetaminus ! p)) (map fst thetaminus))
\end{code}

The update function has two elements: determining the new state and changing the obervables.
We calculte the new state as defined in \cite{danielMasterThesis},
but change the observable management to bespecific to the semantics of a gossip call.

In particular, we do not add the $O^+$ for each agent to their own observables,
but add the \emph{other agent's} $O^+$ to the agent calling with them.
This mimicks the symmetric exchange of secrets. We only do so for the specific two agents in the call.

We furthermore intersect $O^+$ with the (old) state before addng them to an agent's observables.
Agents can only share secrets that they actually know, and if they know a secret, they wil remember it.
This monotonicty of the atoms means that we can safely share observables that are already true,
but not necessarily those that are not true yet.
By intersecting with the state, we ensure only the known secrets get exchanged.

This method is a limited implementation of the effects of a call: in reality,
more information can be inferred and more secret atoms might become observable to an agent.

It seems from our tests that agents cannot learn knowledge that they should not learn, i.e. make formulaes true that should not be.
If this is true, the simple transformer could be a suitable faster computation for simpler questions.
We do note that a mathematical proof of this is required to ascertain such a claim.


\hide{
\begin{code}
instance HasAgents SimpleTransformerWithFactual where
  agentsOf (SimTrfWithF _ _ trfObs) = map fst trfObs

-- There are for now no preconditions
instance HasPrecondition SimpleTransformerWithFactual where
  preOf _ = Top

instance HasPrecondition StwfEvent where
  preOf _ = Top
\end{code}
}