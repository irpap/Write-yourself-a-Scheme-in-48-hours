module Main where

import Data.Char
import Data.Complex
import Data.Ratio
import Data.Array
import SchemeParser
import Test.HUnit

main = runTestTT $ test tests

tests :: Test
tests = TestList $ map TestCase [
          assertEqual
          "parsing a number"
          (Number 33)
          (readExpr "33")
          ,
          assertEqual
          "parsing a complex number"
          (Complex (3.32 :+ 5.0))
          (readExpr "3.32+5i")
          ,
          assertEqual
          "parsing a rational number"
          (Rational (3 % 1))
          (readExpr "6/2")
          ,
          assertEqual
          "Parsing a boolean"
          (Bool True)
          (readExpr "#t")
          ,
          assertEqual
          "parsing a list"
          (List [Number 1,Number 2,Number 3])
           (readExpr "(1 2 3)")
          ,
          assertEqual
          "parsing a dotted list"
          (DottedList [Number 1] (DottedList [Number 2] (DottedList [Number 3] (Atom "nil"))))
          (readExpr "(1 . (2 . (3 . nil)))")
          ,
          assertEqual
          "parsing a quasiquoted expr"
          (List [Atom "quasiquote",List [Number 1,Number 2,List [Atom "unquote",List [Atom "+",Number 3,Number 4]]]])
          (readExpr "`(1 2 ,(+ 3 4))")
          ,
          assertEqual
          "parsing a Vector"
          (Vector (array (0,2) [(0,Number 0),(1,List [Number 2,Number 2,Number 2,Number 2]),(2,String "Anna")]))
          (readExpr "#(0 (2 2 2 2) \"Anna\")")
        ]
