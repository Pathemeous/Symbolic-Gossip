module SimpleTransformer where

import HaitianS5
import SMCDEL.Examples.GossipS5
import SMCDEL.Language
import Data.List ((\\))


simpleGossipInit :: Int -> KnowScene
simpleGossipInit n = (KnS vocab law obs, actual) where
    vocab  = [ hasSof n i j | i <- gossipers n, j <- gossipers n, i /= j ]
    -- State law simply Top, as 'nobody knows other secret' cannot work in later stages
    -- and state law cannot be modified
    -- Might need changing to something else that will remain a law throughout
    law    = boolBddOf Top
    obs    = [ (show i, allSecretsOf n i) | i <- gossipers n ]
    actual = [ ]


simpleGossipTransformer :: Int -> Int -> Int -> SimpleTransformerWithFactual
simpleGossipTransformer n a b = SimTrfWithF eventprops changelaws changeobs where
    thisCallHappens (i,j) = PrpF $ thisCallProp (i,j)
    isInCallForm k = Disj $ [ thisCallHappens (i,k) | i <- gossipers n \\ [k], i < k ]
                        ++ [ thisCallHappens (k,j) | j <- gossipers n \\ [k], k < j ]
    allCalls = [ (i,j) | i <- gossipers n, j <- gossipers n, i < j ]
    -- V+ Event props stay the same as classical transformer
    eventprops = map thisCallProp allCalls
    -- How do we implement the event law? --
    -- Below the law from Classic CallTrf from GossipS5
    --   eventlaw = simplify $
    --     Conj [ Disj (map thisCallHappens allCalls)
    --          -- some call must happen, but never two at the same time:
    --          , Neg $ Disj [ Conj [thisCallHappens c1, thisCallHappens c2]
    --                       | c1 <- allCalls, c2 <- allCalls \\ [c1] ] ]
    -- Theta- Change law stays same as Classic Transformer
    changelaws =
      [(hasSof n i j, boolBddOf $              -- after a call, i has the secret of j iff
          Disj [ has n i j                     -- i already knew j, or
              , Conj (map isInCallForm [i,j]) -- i and j are both in the call or
              , Conj [ isInCallForm i         -- i is in the call and there is some k in
                      , Disj [ Conj [ isInCallForm k, has n k j ] -- the call who knew j
                            | k <- gossipers n \\ [j] ] ]
              ])
      | i <- gossipers n, j <- gossipers n, i /= j ]
      -- set O+ = all the other's secrets for agents a,b and empty for all others
      -- Interleaves the agents a,b to keep the correct ordering of the agents,
      -- which is required for the update-checks (checks agent lists are the same inc order)
    changeobs :: [(Agent, ([Prp], [Prp]))]
    changeobs = [(show k, ([], [])) | k <- gossipers n, k < a ]        ++
                [(show a, (allSecretsOf n b, []))]                     ++
                [(show k, ([], [])) | k <- gossipers n, k > a, k < b ] ++
                [(show b, (allSecretsOf n a, []))]                     ++
                [(show k, ([], [])) | k <- gossipers n, k > b ]

simpleCall :: Int -> (Int,Int) -> StwfEvent
simpleCall n (a,b) = (simpleGossipTransformer n a b, [thisCallProp (a,b)])

doSimpleCall :: KnowScene -> (Int,Int) -> KnowScene
doSimpleCall start (a,b) = start `update` simpleCall (length $ agentsOf start) (a,b)

afterSimple :: Int -> [(Int, Int)] -> KnowScene
afterSimple n = foldl doSimpleCall (simpleGossipInit n)


-- Some helper functions
allSecretsOf :: Int -> Int -> [Prp]
allSecretsOf n x = [ hasSof n x j | j <- gossipers n, j /= x ]