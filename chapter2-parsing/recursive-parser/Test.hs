module Main where

import Data.Char
import Data.Complex
import SchemeParser
import Test.HUnit

main = runTestTT $ test tests

tests :: Test
tests = TestList $ map TestCase [
         assertEqual "tests parsing a complex number" (readExpr "3.32+5i") "Complex (3.32 :+ 5.0)",
         assertEqual "tests parsing a rational number" (readExpr "6/2") "Rational (3 % 1)",
         assertEqual "tests parsing a list" (readExpr "(1 2 3)") "List [Number 1,Number 2,Number 3]",
         assertEqual "tests parsing a dotted list" (readExpr "(a (dotted . list) test)") "List [Atom \"a\",DottedList [Atom \"dotted\"] (Atom \"list\"),Atom \"test\"]"
        ]
