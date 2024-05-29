{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module SmpTrfS5 where

{-
This file is the copy of the SMCDEL fork of Symbolic/S5.hs from Haitian
Modifications:
- Added pattern match for `eval` and `BddOf` for `Dk` and Dkw` (from SMCDEL main)
- Modify the Update instance of Simple Transformers. By definition now the observables are mutated differently:
  O' = O ++ (O+ intersect S) \\ O-
  where S is the newly computed state
  This aims to only add observables when the secret atom is already true,
  which is impossible in the transformer definition

Modified by Malvin:
- deleted most content, replaced by `import SMCDEL.Symbolic.S5`
- only keep the stuff that is specific for simple transformers
- NOTE: this means also the `bddOf` case for simple transformers is gone.
  But as long as you do not need to use dynamic operators in formulas
  (for example to say "agent i knows that after this call we have phi")
  and instead use `update`, this should be sufficient for your application.
-}

import Data.HasCacBDD hiding (Top,Bot)
import Data.List

import SMCDEL.Internal.Help
import SMCDEL.Language
import SMCDEL.Symbolic.S5

-- Simple transformer without factual change

{-
data KnowTransformer = KnTrf
  [Prp]            -- addProps
  Form             -- addLaw
  [(Prp,Bdd)]      -- changeLaw
  [(Agent,[Prp])]  -- addObs
  deriving (Eq,Show)
-}

newtype SimpleTransformerWithoutFactual = SimTrfNoF
  [(Agent,([Prp],[Prp]))]  -- addObs,removeObs
  deriving (Eq,Show)

{-
example:
SimTrfNoF [("Alice", [P 1]),("Bob", [P 1, P 2])]
-}

{-

instance Update KnowScene Event where
  unsafeUpdate (kns@(KnS props _ _),s) (ctrf, eventFactsUnshifted) = (KnS newprops newlaw newobs, news) where
    -- PART 1: SHIFTING addprops to ensure props and newprops are disjoint
    (KnTrf addprops _ changelaw _, shiftrel) = shiftPrepare kns ctrf
    -- the actual event:
    eventFacts = map (apply shiftrel) eventFactsUnshifted
    -- PART 2: COPYING the modified propositions
    changeprops = map fst changelaw
    copyrel = zip changeprops [(freshp $ props ++ addprops)..]
    -- do the pointless update and calculate new actual state
    KnS newprops newlaw newobs = unsafeUpdate kns ctrf
    news = sort $ concat
            [ s \\ changeprops
            , map (apply copyrel) $ s `intersect` changeprops
            , eventFacts
            , filter (\ p -> bddEval (s ++ eventFacts) (changelaw ! p)) changeprops ]

instance Update KnowStruct KnowTransformer where
  checks = [haveSameAgents]
  unsafeUpdate kns ctrf = KnS newprops newlaw newobs where
    (KnS newprops newlaw newobs, _) = unsafeUpdate (kns,undefined::Bdd) (ctrf,undefined::Bdd) -- using laziness!
-}
instance HasAgents SimpleTransformerWithoutFactual where
  agentsOf (SimTrfNoF obs) = map fst obs

-- instance HasPrecondition KnowTransformer where
--   preOf _ = Top

-- When can I do the update:
instance HasPrecondition SimpleTransformerWithoutFactual where
  preOf _ = Top

-- Based on the definition of simple transformer without factual change
{-The first version of defi
instance Update KnowStruct SimpleTransformerWithoutFactual where
  checks = [haveSameAgents]
  unsafeUpdate (KnS v theta obs) (SimTrfNoF addObs rmObs) = KnS v theta newobs
    where newobs      = zipWith g (zipWith f obs addObs) rmObs
          f ob addob   = (fst ob, snd ob ++ snd addob)
          g fObAddob rmob = (fst fObAddob, snd fObAddob \\ snd rmob)
-}
--The second definition of defi
instance Update KnowStruct SimpleTransformerWithoutFactual where
  checks = [haveSameAgents]
  unsafeUpdate (KnS v theta obs) (SimTrfNoF trfObs) = KnS v theta newobs
    where newobs = [ (ag, nub (ob ++ fst (trfObs ! ag)) \\ snd (trfObs ! ag)) | (ag,ob) <- obs ]
--  should I consider removing the duplicates for (ob ++ (addObs ! ag))?

instance Update KnowScene SimpleTransformerWithoutFactual where
  checks = [haveSameAgents]
  unsafeUpdate (kns,s) sTNF = (unsafeUpdate kns sTNF,s)




-- instance Pointed KnowTransformer State
-- type Event = (KnowTransformer,State)

-- instance HasPrecondition Event where
--   preOf (KnTrf addprops addlaw _ _, x) = simplify $ substitOutOf x addprops addlaw


-- Simple transformer with factual change
data SimpleTransformerWithFactual = SimTrfWithF
  [Prp]            -- V+ is a set of new variables encoding of a set of events
  [(Prp,Bdd)]      -- theta- assigns a formula to each modified variable. -- V- (simply the domain of theta-) determines all variables which values may be changed.
  -- [(Agent,[Prp])]  -- Oi determines which events an agent can distinguish -- removed, but wait for Dniels email
  [(Agent,([Prp],[Prp]))]  -- trfObs
  deriving (Eq,Show)

instance Pointed SimpleTransformerWithFactual State
type StwfEvent = (SimpleTransformerWithFactual,State)

--May need to modify this part:
instance HasPrecondition StwfEvent where
  preOf _ = Top

instance Update KnowStruct SimpleTransformerWithFactual where
  checks = [haveSameAgents]
  unsafeUpdate :: KnowStruct -> SimpleTransformerWithFactual -> KnowStruct
  unsafeUpdate kns@KnS {} (SimTrfWithF _ _ trfObs) = KnS newprops newlaw newobs where
    KnS newprops newlaw newobs = unsafeUpdate kns (SimTrfNoF trfObs)

-- Modified to be specific to Gossip Calls
instance Update KnowScene StwfEvent where
  checks = [haveSameAgents]
  unsafeUpdate (KnS v th obs,s) (SimTrfWithF _ thetaminus _,x) = (newkns, newstate) where
    -- copy of gossip helper functions
    -- to be able to find the current two agents in the call
    thisCallProp :: (Int,Int) -> Prp
    thisCallProp (i,j) | i < j     = P (100 + 10*i + j)
                      | otherwise = error $ "wrong call: " ++ show (i,j)
    n = length obs
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

-- May need to modify this part to add more preconditions
instance HasPrecondition SimpleTransformerWithFactual where
  preOf _ = Top
