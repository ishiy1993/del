module Main where

import System.Exit

import Encode
import Format
import Parser
import Lib

main :: IO ()
main = do
    meom <- parseEOMFromFile "euler1.txt"
    case meom of
         Nothing -> die "Error"
         Just eom -> do
             putStrLn $ encode eom
             putStrLn $ encode $ diffByT eom
             putStrLn $ encode $ diffByX eom
             putStrLn $ encode $ diffByX $ diffByT eom

