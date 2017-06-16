module Main where

import Lib

main :: IO ()
main = do
    meom <- parseEOMFromFile "euler1.txt"
    putStrLn "Input EOM:"
    putStrLn $ maybe "Error" formatEOM meom
    putStrLn ""
    putStrLn "Output:"
    putStrLn $ maybe "Error" formatEOM $ differentiatedByT <$> meom
