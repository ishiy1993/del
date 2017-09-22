module Main where

import Control.Monad
import System.Environment
import System.Exit

import Format
import Parser

main :: IO ()
main = do
    as <- getArgs
    when (null as) $ die "Need a file name"
    eom <- getEOMFromFile (head as)
    putStr $ toCode eom
