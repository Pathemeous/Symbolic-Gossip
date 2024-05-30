module Main where

import Data.Maybe 
import Text.Read
import SMCDEL.Examples.GossipS5
import SMCDEL.Symbolic.S5
import Explain
import Transparent 
import SimpleTransformer 

main :: IO ()
main = do
    putStrLn "Gossip Scene Investigation"
    putStrLn "=========================="
    putStr "Enter the number of agents: "
    line <- getLine
    let n = readMaybe line
    if isNothing n
        then putStrLn "Error: input was not recognized as a number, please try again"
        else do 
             putStrLn "Enter the code corresponding to which transformer you want to use:"
             putStrLn "     (T) Transparent     (S) Synchronous (SMCDEL)     (Si) Simple"
             transformer <- getLine 
             if transformer == "T" then mainHelperT (fromJust n) (gossipInit $ fromJust n)
             else if transformer == "S" then mainHelperS (fromJust n) (gossipInit $ fromJust n)
             else if transformer == "Si" then mainHelperSi (fromJust n) (simpleGossipInit $ fromJust n)
             else putStrLn "Error: input did not correspond to a transformer, please try again"


mainHelperT :: Int -> KnowScene -> IO ()
mainHelperT n ks = do
    putStrLn "Input which two agents call"
    putStr   "First agent: "
    line <- getLine
    let a = readMaybe line
    if isNothing a || fromJust a >= n 
        then do 
             putStrLn "Error: unrecognized agent. Retrying..." 
             mainHelperT n ks
        else do 
            putStrLn "Input which two agents call"
            putStr   "Second agent: "
            line' <- getLine
            let b = readMaybe line'
            if isNothing b || fromJust b >= n 
                then do 
                    putStrLn "Error: unrecognized agent. Retrying..." 
                    mainHelperT n ks
                else do 
                    let ks' = doCallTransparent ks (fromJust a, fromJust b)
                    gsi ks' 
                    mainHelperT n ks' 


mainHelperS :: Int -> KnowScene -> IO ()
mainHelperS n ks = do
    putStrLn "Input which two agents call"
    putStr   "First agent: "
    line <- getLine
    let a = readMaybe line
    if isNothing a || fromJust a >= n 
        then do 
             putStrLn "Error: unrecognized agent. Retrying..." 
             mainHelperT n ks
        else do 
            putStrLn "Input which two agents call"
            putStr   "Second agent: "
            line' <- getLine
            let b = readMaybe line'
            if isNothing b || fromJust b >= n 
                then do 
                    putStrLn "Error: unrecognized agent. Retrying..." 
                    mainHelperT n ks
                else do 
                    let ks' = doCall ks (fromJust a, fromJust b)
                    gsi ks' 
                    mainHelperT n ks' 


mainHelperSi :: Int -> KnowScene -> IO ()
mainHelperSi n ks = do
    putStrLn "Input which two agents call"
    putStr   "First agent: "
    line <- getLine
    let a = readMaybe line
    if isNothing a || fromJust a >= n 
        then do 
             putStrLn "Error: unrecognized agent. Retrying..." 
             mainHelperT n ks
        else do 
            putStrLn "Input which two agents call"
            putStr   "Second agent: "
            line' <- getLine
            let b = readMaybe line'
            if isNothing b || fromJust b >= n 
                then do 
                    putStrLn "Error: unrecognized agent. Retrying..." 
                    mainHelperT n ks
                else do 
                    let ks' = doSimpleCall ks (fromJust a, fromJust b)
                    gsi ks' 
                    mainHelperT n ks' 
