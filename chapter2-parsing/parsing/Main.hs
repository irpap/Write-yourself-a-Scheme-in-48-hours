module Main where

import SchemeParser

import System.Environment

main = do
    args <- getArgs
    putStrLn(readExpr $ args !! 0)
