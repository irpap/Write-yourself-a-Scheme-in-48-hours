module Main where

import Data.Char
import Data.Complex
import SchemeParser
import Test.HUnit

main = runTestTT $ test tests

tests :: Test
tests = TestList $ map TestCase [
    assertEqual "tests parsing a complex number" (readExpr "3.32+5i") "Complex (3.32 :+ 5.0)",
    assertEqual "tests parsing a rational number" (readExpr "6/2") "Rational (3 % 1)"]
