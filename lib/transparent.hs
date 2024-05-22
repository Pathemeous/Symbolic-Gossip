module Transparent where

import SMCDEL.Examples.GossipS5
import SMCDEL.Language
import SMCDEL.Symbolic.S5
import Data.List

--import Debug.Trace

callTrfTransparent :: Int -> Int -> Int -> KnowTransformer
callTrfTransparent n a b = KnTrf eventprops eventlaw changelaws eventobs where
  -- agent k is in the call ab if a calls k (so k==b) or k calls b (so k==a)
  isInCallForm k | k == a = Top 
                 | k == b = Top 
                 | otherwise = Bot
                 
  thisCallHappens = thisCallProp (a,b)
  -- callPropsWith k = [ thisCallProp (i,k) | i <- gossipers n, i < k ]
  --               ++ [ thisCallProp (k,j) | j <- gossipers n, k < j ]
  eventprops = [thisCallHappens]
  -- call ab takes place and no other calls happen
  eventlaw = Conj [PrpF $ thisCallHappens,
                 Conj [Neg (PrpF $ thisCallProp (i,j)) | i <- gossipers n, j <- gossipers n, not ((i == a && j == b) || (i == b && j == a)), i < j ]]
  changelaws =
    [(hasSof n i j, boolBddOf $             
        Disj [ has n i j                     
             , Conj (map isInCallForm [i,j]) 
             , Conj [ isInCallForm i         
                    , Disj [ Conj [ isInCallForm k, has n k j ] 
                           | k <- gossipers n \\ [j], a<k && k<b ]]
             ])
    | i <- gossipers n, j <- gossipers n, i /= j ]
  eventobs = [(show k, [thisCallHappens]) | k <- gossipers n]


  -- check out PrpF
  -- changelog
        -- thisCallHappens
        -- isInCallForm 
        -- eventProps
        -- should eventobs be the same or tailored to ab? currently: tailored to ab

callTransparent :: Int -> (Int,Int) -> Event
callTransparent n (a,b) = (callTrfTransparent n a b, [thisCallProp (a,b)])

doCallTransparent :: KnowScene -> (Int,Int) -> KnowScene
doCallTransparent start (a,b) = start `update` callTransparent (length $ agentsOf start) (a,b)

afterTransparent :: Int -> [(Int,Int)] -> KnowScene
afterTransparent n = foldl doCallTransparent (gossipInit n)

isSuccessTransparent :: Int -> [(Int,Int)] -> Bool
isSuccessTransparent n cs = evalViaBdd (afterTransparent n cs) (allExperts n)

