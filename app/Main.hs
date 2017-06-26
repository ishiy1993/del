module Main where

import Control.Monad
import System.Environment
import System.Exit

import Encode
import Format
import Parser

main :: IO ()
main = do
    as <- getArgs
    when (null as) $ die "Need a file name"
    meom <- parseEOMFromFile $ head as
    maybe (die "Unable to parse") (putStr . toCode) meom
