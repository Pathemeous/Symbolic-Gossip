module Explain where
   
import SMCDEL.Examples.GossipS5
import SMCDEL.Symbolic.S5
import SMCDEL.Language
import SMCDEL.Other.BDD2Form
import Test.QuickCheck
import Data.Maybe

-- in  : number of gossipers (Int), offset that distinguishes "secret" props from "call" props (Int), and the actual prop to decipher (Prp).
-- out : String containing meaning of proposition (String)
explainPrp :: Int -> Prp -> String
explainPrp n (P prop) | i < n && j < n && i /= j = "S_{"++ show i ++ "}"++ show j
                      | otherwise = otherProp n (P prop)
                              where 
                              i = prop `quot` n
                              j = prop `rem` n
                              otherProp :: Int -> Prp -> String
                              otherProp n' (P prop') = show $ prop' - (n'* n' -n' )


prpLibrary :: [Prp] -> Int -> [(Prp,String)]
prpLibrary prps n = zip prps (prpLibraryHelper prps "")
   where 
      prpLibraryHelper :: [Prp]  -> String -> [String]
      prpLibraryHelper [] _ = []
      prpLibraryHelper prps' r = secretDecoder 0 (take (n*(n-1)) prps') r ++ callDecoder 0 (take (div (n*(n-1)) 2) (drop (n*(n-1)) prps')) r ++ prpLibraryHelper (drop (div (3*n*(n-1)) 2) prps') (r ++ "'")
      secretDecoder :: Int -> [Prp] -> String -> [String]
      secretDecoder k secrets r' |  k >= n*(n-1) + n = []
                                 |  otherwise = ("S_{"++ show i ++ "}"++ show j ++ r') : secretDecoder (k + 1) secrets r'
         where 
            (i,j) = (k `quot` n, k `rem` n)
      callDecoder :: Int -> [Prp] -> String -> [String]
      callDecoder k calls r' | k >= div (n*(n-1)) 2 = []
                             | null calls = []
                             | otherwise = ("q" ++ show i ++ show j ++ r') : callDecoder (k + 1) calls r'
         where 
            (i, j) = getCNums k 0
            getCNums :: Int -> Int -> (Int,Int)
            getCNums k' r'' | (k'+1) < n = (r'',k'+1)
                            | otherwise = getCNums (k'-n+2+r'') (r''+1)
         
explainPrps :: Prp -> [(Prp,String)] -> String
explainPrps (P x) prpLib = fromJust (lookup (P x) prpLib)

-- Alright so the case with offset does not yet work. This is because the update structure may contain propositions of this form
-- But after the update the propositions just turn into a list of propositions increasing (+1) in order. 
-- a call adds propositions for all possible calls and copies the entire previous vocab. 
-- For inctance: 
-- ghci> s0 = gossipInit 3
-- ghci> s0
-- (KnS [P 1,P 2,P 3,P 5,P 6,P 7] Var 1 Bot (Var 2 Bot (Var 3 Bot (Var 5 Bot (Var 6 Bot (Var 7 Bot Top))))) [("0",[]),("1",[]),("2",[])],[])
-- ghci> doCall s0 (0,1) 
-- (KnS [P 1,P 2,P 3,P 5,P 6,P 7,  P 8,P 9,P 10,   P 11,P 12,P 13,P 14,P 15,P 16] 
--                                 (calls)         (copy) 
--       Var 1 (Var 2 Bot (Var 3 (Var 5 Bot (Var 6 Bot (Var 7 Bot (Var 8 (Var 9 Bot (Var 10 Bot (Var 11 Bot (Var 12 Bot (Var 13 Bot (Var 14 Bot (Var 15 Bot (Var 16 Bot Top)))))))) Bot)))) Bot)) (Var 2 (Var 3 Bot (Var 5 Bot (Var 6 (Var 7 Bot (Var 8 Bot (Var 9 (Var 10 Bot (Var 11 Bot (Var 12 Bot (Var 13 Bot (Var 14 Bot (Var 15 Bot (Var 16 Bot Top))))))) Bot))) Bot))) (Var 3 Bot (Var 5 (Var 6 Bot (Var 7 (Var 8 Bot (Var 9 Bot (Var 10 (Var 11 Bot (Var 12 Bot (Var 13 Bot (Var 14 Bot (Var 15 Bot (Var 16 Bot Top)))))) Bot))) Bot)) Bot)))
--      [("0",[P 8,P 9]),("1",[P 8,P 10]),("2",[P 9,P 10])],
--  [P 1,   P 3,   P 8])
--   S_01   S_10   q_01


kSProps :: KnowStruct -> Int -> IO ()
kSProps (KnS voc sLaw obs) n = do 
   putStrLn "vocab: "
   mapM_ (putStrLn . (++) " --  " . explainPrp n) voc
   putStrLn "State Law: "
   -- Translate BDS back to formula. 
   -- Can be made a bit nicer, maybe make it latex or smth. 
   -- Still needs the propositions in the formula translated/explained
   print (formOf sLaw)
   putStrLn "Observables: "
   mapM_ (putStrLn . (++) " --  " . (\ x -> fst x ++ ":  " ++ show (map (explainPrp 3) (snd x)) )) obs

-- GOSSIP SCENE INVESTIGATION   ...get it? 
gsi :: KnowScene -> Int -> IO ()
gsi (ks, s) n = do 
   kSProps ks n
   putStrLn "actual state: "
   if null s then putStrLn " --  Nothing is true" 
      else 
      mapM_ (putStrLn . (++) " --  " . explainPrp n) s
