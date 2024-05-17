module Explain where

import SMCDEL.Examples.GossipS5
import SMCDEL.Language
import Test.QuickCheck

explainCallPrp :: Prp -> String
explainCallPrp (P call) = "Agent "++ (show i) ++ " called with agent " ++ (show j)
   where 
    i = (call - 100) `quot` 10
    j = (call - 100) `rem` 10

