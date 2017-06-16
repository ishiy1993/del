module Main where

import Lib

main :: IO ()
main = do
    meom <- parseEOMFromFile "euler1.txt"
    case meom of
         Nothing -> putStrLn "Error"
         Just eom -> putStrLn $ formatEOM eom
