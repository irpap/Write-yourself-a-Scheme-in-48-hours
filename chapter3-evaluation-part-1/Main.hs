module Main where

import SchemeParser

import System.Environment

main = getArgs >>= print . readExpr . head
