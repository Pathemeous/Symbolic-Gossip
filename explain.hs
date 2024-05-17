module Explain where

import SMCDEL.Examples.GossipS5
import SMCDEL.Language
import Test.QuickCheck

explainCallPrp :: Int -> Prp -> String
explainCallPrp n (P call) = "Agent " ++ (show i) ++ " called with agent " ++ (show j)
   where 
    i = (call - 100) `quot` n
    j = (call - 100) `rem` n

