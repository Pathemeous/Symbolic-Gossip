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

import Data.HasCacBDD hiding (Top,Bot)
import Data.List

import SMCDEL.Internal.Help
import SMCDEL.Language
import SMCDEL.Symbolic.S5


{-
    Simple transformer without factual change
-}

newtype SimpleTransformerWithoutFactual = SimTrfNoF
  [(Agent,([Prp],[Prp]))]  -- O+ and O- for each agent
  deriving (Eq,Show)


instance HasAgents SimpleTransformerWithoutFactual where
  agentsOf (SimTrfNoF obs) = map fst obs

instance HasPrecondition SimpleTransformerWithoutFactual where
  preOf _ = Top

instance Update KnowStruct SimpleTransformerWithoutFactual where
  checks = [haveSameAgents]
  unsafeUpdate (KnS v theta obs) (SimTrfNoF trfObs) = KnS v theta newobs
    where newobs = [ (ag, nub (ob ++ fst (trfObs ! ag)) \\ snd (trfObs ! ag)) | (ag,ob) <- obs ]

instance Update KnowScene SimpleTransformerWithoutFactual where
  checks = [haveSameAgents]
  unsafeUpdate (kns,s) sTNF = (unsafeUpdate kns sTNF,s)


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

instance HasPrecondition StwfEvent where
  preOf _ = Top

instance Update KnowStruct SimpleTransformerWithFactual where
  checks = [haveSameAgents]
  unsafeUpdate :: KnowStruct -> SimpleTransformerWithFactual -> KnowStruct
  unsafeUpdate kns@KnS {} (SimTrfWithF _ _ trfObs) = KnS newprops newlaw newobs where
    KnS newprops newlaw newobs = unsafeUpdate kns (SimTrfNoF trfObs)

-- The following instance is modified from Haitian's implementation of the
-- general simple transformer definition
-- It is -only- sound for synchronous Gossip calls
instance Update KnowScene StwfEvent where
  checks = [haveSameAgents]
  unsafeUpdate kns@(KnS v th obs,s) (SimTrfWithF _ thetaminus _,x) = (newkns, newstate) where
    -- gossip helper functions to be able to find the current two agents in the call
    thisCallProp :: (Int,Int) -> Prp
    thisCallProp (i,j) | i < j     = P (100 + 10*i + j)
                      | otherwise = error $ "wrong call: " ++ show (i,j)

    n = length $ agentsOf kns
    gossipers :: [Int]
    gossipers = [0..(n-1)]

    allCalls = [ (i,j) | i <- gossipers, j <- gossipers, i < j ]
    allCallprops = map thisCallProp allCalls
    callPropResolver = zip allCallprops allCalls

    -- the transformation state x contains only 1 call proposition
    inThisCall :: (Int, Int)
    inThisCall = callPropResolver ! head x

    -- Compute special observable management for Gossip
    -- Calling agents get their own original observables O_i plus
    -- the intersection of the other agent's observables with the state (their known true secrets)
    -- Note that the transformer observables are ignored fully.
    newobs = [ (show i,obs ! show i) | i <- gossipers, i < fst inThisCall ] ++
             [ (show (fst inThisCall) , obs ! show (fst inThisCall) ++ intersect newstate (obs ! show (snd inThisCall))) ] ++
             [ (show i,obs ! show i) | i <- gossipers, i > fst inThisCall, i < snd inThisCall ] ++
             [ (show (snd inThisCall) , obs ! show (snd inThisCall) ++ intersect newstate (obs ! show (fst inThisCall))) ] ++
             [ (show i,obs ! show i) | i <- gossipers, i > snd inThisCall  ]


    newkns = KnS v th newobs -- keep V and Theta but changes obs
    newstate = sort ((s \\ map fst thetaminus) ++ filter (\ p -> bddEval (s ++ x) (thetaminus ! p)) (map fst thetaminus))

instance HasAgents SimpleTransformerWithFactual where
  agentsOf (SimTrfWithF _ _ trfObs) = map fst trfObs

-- There are for now no preconditions
instance HasPrecondition SimpleTransformerWithFactual where
  preOf _ = Top