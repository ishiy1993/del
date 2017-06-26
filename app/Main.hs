module Main where

import Control.Monad
import System.Environment
import System.Exit

import Encode
import Format
import Parser
import Lib

main :: IO ()
main = do
    as <- getArgs
    when (null as) $ die "Need a file name"
    meom <- parseEOMFromFile $ head as
    case meom of
         Nothing -> die "Unable to parse"
         Just eom -> do
             putStrLn $ encode eom
             putStrLn $ encode $ diffByT eom
             putStrLn $ encode $ diffByX eom
             putStrLn $ encode $ diffByX $ diffByT eom

