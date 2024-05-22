module SimpleTrf where 

import SMCDEL.Examples.GossipS5 
import SMCDEL.Language
import SMCDEL.Symbolic.S5 
import Data.List
import SMCDEL.Symbolic.S5_DD (Bdd)

-- Everything from Haitian that we can use 
-- From SMCDEL-Hanabi/src/SMCDEL/Symbolic/S5.hs  (bottom of file)



-- Simple transformer with factual change
data SimpleTransformerWithFactual = SimTrfWithF 
  [Prp]                    -- V+ is a set of new variables encoding of a set of events            
  [(Prp,Bdd)]              -- theta- assigns a formula to each modified variable. -- V- (simply the domain of theta-) determines all variables which values may be changed.
  -- [(Agent,[Prp])]       -- Oi determines which events an agent can distinguish -- removed, but wait for Daniels email
  [(Agent,([Prp],[Prp]))]  -- trfObs
  deriving (Eq,Show)

-- instance Pointed SimpleTransformerWithFactual State  -- use [Prp] instead of State? 
-- type StwfEvent = (SimpleTransformerWithFactual,State)


--May need to modify this part:
-- instance HasPrecondition StwfEvent where
--   preOf _ = Top



-- instance Update KnowStruct SimpleTransformerWithFactual where
--   checks = [haveSameAgents]
--   unsafeUpdate kns@KnS {} (SimTrfWithF _ _ trfObs) = KnS newprops newlaw newobs where
--     KnS newprops newlaw newobs = unsafeUpdate kns (SimTrfNoF trfObs)



-- instance Update KnowScene StwfEvent where
--   checks = [haveSameAgents]
--   unsafeUpdate (kns@KnS {},s) (stwf@(SimTrfWithF _ thetaminus _),x) = (newkns, news) where
--     newkns = unsafeUpdate kns stwf
--     news = sort ((s \\ map fst thetaminus) ++ filter (\ p -> bddEval (s ++ x) (thetaminus ! p)) (map fst thetaminus))

-- check line 368-373 code above and the corresponding definition pp.65 of Malvin's thesis (comment from Haitian)

instance HasAgents SimpleTransformerWithFactual where
  agentsOf (SimTrfWithF _ _ trfObs) = map fst trfObs

-- May need to modify this part to add more preconditions
instance HasPrecondition SimpleTransformerWithFactual where
  preOf _ = Top