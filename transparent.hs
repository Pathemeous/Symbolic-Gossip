module Transparent where 

import SMCDEL.Examples.GossipS5
import SMCDEL.Language
import SMCDEL.Symbolic.S5
import Data.List

callTrfTransparent :: Int -> Int -> Int -> KnowTransformer
callTrfTransparent n a b = KnTrf eventprops eventlaw changelaws eventobs where
  -- agent k is in the call ab if a calls k (so k==b) or k calls b (so k==a)
  isInCallForm k = Disj $ [ PrpF $ thisCallProp (a,k), PrpF $ thisCallProp (k,b) ]
  thisCallHappens = thisCallProp (a,b)
  callPropsWith k = [ thisCallProp (i,k) | i <- gossipers n, i < k ]
                ++ [ thisCallProp (k,j) | j <- gossipers n, k < j ]
  eventprops = [thisCallHappens]
  -- call ab takes place and no other calls happen
  eventlaw = Conj [PrpF $ thisCallHappens, 
                 (Conj [(Neg (PrpF $ thisCallProp (i,j))) | i <- gossipers n
                                                          , j <- gossipers n 
                                                          , not ((i == a && j == b) || (i == b && j == a))
                                                          , i < j ])]
  changelaws = 
    [(hasSof n i j, boolBddOf $              -- after a call, i has the secret of j iff
        Disj [ has n i j                     -- i already knew j, or
             , Conj (map isInCallForm [i,j]) -- i and j are both in call ab or
             , Conj [ isInCallForm i         -- i is in the call and there is some k in
                    , Disj [ Conj [ isInCallForm k, has n k j ] -- the call who knew j
                           | k <- gossipers n \\ [j]]]
             ])
    | i <- gossipers n, j <- gossipers n, i /= j ] 
  eventobs = [(show k, callPropsWith k) | k <- gossipers n]


  -- check out PrpF
  -- changelog
        -- thisCallHappens
        -- isInCallForm 
        -- eventProps
        -- should eventobs be the same or tailored to ab?

callTransparent :: Int -> (Int,Int) -> Event 
callTransparent n (a,b) = (callTrfTransparent n a b, [thisCallProp (a,b)])

doCallTransparent :: KnowScene -> (Int,Int) -> KnowScene
doCallTransparent start (a,b) = start `update` callTransparent (length $ agentsOf start) (a,b)

afterTransparent :: Int -> [(Int,Int)] -> KnowScene
afterTransparent n = foldl doCallTransparent (gossipInit n)

isSuccessTransparent :: Int -> [(Int,Int)] -> Bool
isSuccessTransparent n cs = evalViaBdd (afterTransparent n cs) (allExperts n)

whoKnowsMeta :: KnowScene -> [(Int,[(Int,String)])]
whoKnowsMeta scn = [ (k, map (meta k) [0..maxid] ) | k <- [0..maxid] ] where
  n = length (agentsOf scn)
  maxid = n - 1
  meta x y = (y, map (knowsAbout x y) [0..maxid])
  knowsAbout x y i
    | y == i = 'X'
    | evalViaBdd scn (      K (show x) $       PrpF (hasSof n y i)) = 'Y'
    | evalViaBdd scn (Neg $ K (show x) $ Neg $ PrpF (hasSof n y i)) = '?'
    | evalViaBdd scn (      K (show x) $ Neg $ PrpF (hasSof n y i)) = '_'
    | otherwise                                                     = 'E'

