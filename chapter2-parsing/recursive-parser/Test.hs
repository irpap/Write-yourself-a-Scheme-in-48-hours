module Main where

import Data.Char
import Data.Complex
import SchemeParser
import Test.HUnit

main = runTestTT $ test tests

tests :: Test
tests = TestList $ map TestCase [
          assertEqual
          "tests parsing a complex number"
          "Complex (3.32 :+ 5.0)"
          (readExpr "3.32+5i")
          ,
          assertEqual
          "tests parsing a rational number"
          "Rational (3 % 1)"
          (readExpr "6/2")
          ,
          assertEqual
          "tests parsing a list"
          "List [Number 1,Number 2,Number 3]"
           (readExpr "(1 2 3)")
          ,
          assertEqual
          "tests parsing a dotted list"
          "List [Atom \"a\",DottedList [Atom \"dotted\"] (Atom \"list\"),Atom \"test\"]"
          (readExpr "(a (dotted . list) test)")
          ,
          assertEqual
          "tests parsing a quasiquoted expr"
          "List [Atom \"quasiquote\",List [Number 1,Number 2,List [Atom \"unquote\",List [Atom \"+\",Number 3,Number 4]]]]"
          (readExpr "`(1 2 ,(+ 3 4))")
        ]
