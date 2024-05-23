{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
module SimpleTrf where 

import SMCDEL.Examples.GossipS5 
import SMCDEL.Language
import SMCDEL.Symbolic.S5 (bddOf) 
import SMCDEL.Symbolic.S5_DD 
import SMCDEL.Internal.Help

import Data.Dynamic
import Data.List
import Data.Maybe
import Data.HasCacBDD hiding (Top,Bot,Bdd) 


-- Everything from Haitian that we can use 
-- From SMCDEL-Hanabi/src/SMCDEL/Symbolic/S5.hs  (bottom of file)


-- WITHOUT FACTUAL CHANGE (used later in with factual change)

newtype SimpleTransformerWithoutFactual = SimTrfNoF
  [(Agent,([Prp],[Prp]))]  -- addObs,removeObs
  deriving (Eq,Show)

instance HasAgents SimpleTransformerWithoutFactual where
  agentsOf (SimTrfNoF obs) = map fst obs

-- When can I do the update:
instance HasPrecondition SimpleTransformerWithoutFactual where
  preOf _ = Top

--The second definition of defi
instance Update SMCDEL.Symbolic.S5_DD.KnowStruct SimpleTransformerWithoutFactual where
  checks = [haveSameAgents]
  unsafeUpdate (KnS v theta obs) (SimTrfNoF trfObs) = KnS v theta newobs
    where newobs = [ (ag, nub (ob ++ fst (trfObs ! ag)) \\ snd (trfObs ! ag)) | (ag,ob) <- obs ]
--  should I consider removing the duplicates for (ob ++ (addObs ! ag))?

instance Update KnowScene SimpleTransformerWithoutFactual where
  checks = [haveSameAgents]
  unsafeUpdate (kns,s) sTNF = (unsafeUpdate kns sTNF,s)


--  WITH FACTUAL CHANGE

-- Simple transformer with factual change
data SimpleTransformerWithFactual = SimTrfWithF 
  [Prp]                    -- V+ is a set of new variables encoding of a set of events            
  [(Prp,Bdd)]              -- theta- assigns a formula to each modified variable. -- V- (simply the domain of theta-) determines all variables which values may be changed.
  -- [(Agent,[Prp])]       -- Oi determines which events an agent can distinguish -- removed, but wait for Daniels email
  [(Agent,([Prp],[Prp]))]  -- trfObs
  deriving (Eq,Show)


instance Pointed SimpleTransformerWithFactual State   -- use [Prp] instead of State? 
type StwfEvent = (SimpleTransformerWithFactual,State)

-- May need to modify this part:
instance HasPrecondition StwfEvent where
  preOf _ = Top


-- Simple Transformers with factual change
-- bddOf kns (Dia (Dyn dynLabel d) f) | isJust (fromDynamic d :: Maybe StwfEvent) =
--     -- TODO  relabelWith copyrelInverse              -- 4. Copy back changeProps V_-^o to V_-
--       simulateActualEvents                    -- 3. Simulate actual event(s) [see below]
--     . substitSimul [ (k, changeLaw ! p)       -- 2. Replace changeProps V_ with postcons
--                    | p@(P k) <- changeProps]  --    (no "relabelWith copyrel", undone in 4)
--     . SMCDEL.Symbolic.S5.bddOf (kns `update` trf)                -- 1. boolean equivalent wrt new struct
--     $ f
--   where
--     changeProps = map fst changeLaw
--     copychangeProps = [(freshp $ vocabOf kns ++ addProps)..]
--     copyrelInverse  = zip copychangeProps changeProps
--     trf@(SimTrfWithF addProps changeLaw _trfObs) = trfUnshifted -- TODO shiftPrepare kns
--     (trfUnshifted,simulateActualEvents) =
--       case fromDynamic d of
--         -- 3. For a single event, simulate actual event x outof V+
--         Just ((t,x) :: StwfEvent) -> ( t, (`restrictSet` [(fromEnum k, k `elem` x) | k <- addProps] ))
--           -- TODO where actualAss = [(newK, P k `elem` x) | (P k, P newK) <- shiftrel]
--         -- (NOTE: no multipointed StwfEVent for now.)
--         Nothing -> error $ "cannot update knowledge structure with '" ++ dynLabel ++ "':\n " ++ show d



instance Update SMCDEL.Symbolic.S5_DD.KnowStruct SimpleTransformerWithFactual where
  checks = [haveSameAgents]
  unsafeUpdate kns@KnS {} (SimTrfWithF _ _ trfObs) = KnS newprops newlaw newobs where
    KnS newprops newlaw newobs = unsafeUpdate kns (SimTrfNoF trfObs)



instance Update KnowScene StwfEvent where
  checks = [haveSameAgents]
  unsafeUpdate (kns@KnS {},s) (stwf@(SimTrfWithF _ thetaminus _),x) = (newkns, news) where
    newkns = unsafeUpdate kns stwf
    news = sort ((s \\ map fst thetaminus) ++ filter (\ p -> bddEval (s ++ x) (thetaminus ! p)) (map fst thetaminus))

-- check line 368-373 code above and the corresponding definition pp.65 of Malvin's thesis (comment from Haitian)

instance HasAgents SimpleTransformerWithFactual where
  agentsOf :: SimpleTransformerWithFactual -> [Agent]
  agentsOf (SimTrfWithF _ _ trfObs) = map fst trfObs

-- May need to modify this part to add more preconditions
instance HasPrecondition SimpleTransformerWithFactual where
  preOf _ = Top