module Explain where

import SMCDEL.Examples.GossipS5
import SMCDEL.Symbolic.S5
import SMCDEL.Language
import SMCDEL.Other.BDD2Form
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

kSProps :: KnowStruct -> Int -> Int -> IO ()
kSProps (KnS voc sLaw obs) n offset = do 
   putStrLn "vocab: "
   mapM_ (putStrLn . ((++) " --  ") . (explainPrp n offset)) voc
   putStrLn "State Law: "
   -- Translate BDS back to formula. 
   -- Can be made a bit nicer, maybe make it latex or smth. 
   putStrLn (show $ formOf sLaw)
   putStrLn "Observables: "
   mapM_ (putStrLn . ((++) " --  ") . (\ x -> (fst x) ++ ":  " ++ show (snd x) )) obs

-- crime SCENE INVESTIGATION   ...get it? 
csi :: KnowScene -> Int -> Int -> IO ()
csi (ks, s) n offset = do 
   kSProps ks n offset 
   putStrLn "actual state: "
   if s == [] then putStrLn " --  Nothing is true" 
      else 
      mapM_ (putStrLn . ((++) " --  ") . (explainPrp n offset)) s

