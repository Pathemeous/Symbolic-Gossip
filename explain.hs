module Explain where

import SMCDEL.Examples.GossipS5
import SMCDEL.Symbolic.S5
import SMCDEL.Language
import Test.QuickCheck

-- in  : number of gossipers (Int), offset that distinguishes "secret" props from "call" props (Int), and the actual prop to decipher (Prp).
-- out : String containing meaning of proposition (String)
explainPrp :: Int -> Int -> Prp -> String
explainPrp n offset (P prop) | prop > offset = "Agent " ++ (show i) ++ " called with agent " ++ (show j)
                                 | otherwise     = "S_{"++(show i') ++ "}"++ (show j')
                                    where 
                                    i  = (prop - offset) `quot` 10
                                    j  = (prop - offset) `rem` 10
                                    i' = prop `quot` n
                                    j' = prop `rem` n


-- explainKS :: KnowStruct -> Int -> Int -> String 
-- explainKS (KnS voc sLaw obs) n offset = vocString 
--    where vocString =

kSProps :: KnowStruct -> Int -> Int -> IO ()
kSProps (KnS voc sLaw obs) n offset = do 
   putStrLn "vocab: "
   mapM_ (putStrLn . ((++) " --  ") . (explainPrp n offset)) voc
   putStrLn "State Law: "
   putStrLn "Observables: "

