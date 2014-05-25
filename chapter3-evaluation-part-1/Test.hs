module Main where

import Data.Char
import Data.Complex
import SchemeParser
import Test.HUnit

main = runTestTT $ test tests

tests :: Test
tests = TestList $ map TestCase [
          assertEqual
          "parsing a number"
          "33"
          (show $ readExpr "33")
          ,
          assertEqual
          "parsing a complex number"
          "3.32 :+ 5.0"
          (show $ readExpr "3.32+5i")
          ,
          assertEqual
          "parsing a rational number"
          "3 % 1"
          (show $ readExpr "6/2")
          ,
          assertEqual
          "parsing a list"
          "(1 2 3)"
           (show $ readExpr "(1 2 3)")
          ,
          assertEqual
          "parsing a dotted list"
          "(1.(2.(3.nil)))"
          (show $ readExpr "(1 . (2 . (3 . nil)))")
          ,
          --assertEqual
          --"parsing a quasiquoted expr"
          --"(quasiquote (1 2 (unquote (+ 3 4))))"
          --(show $ readExpr "`(1 2 ,(+ 3 4))")
          --,
          assertEqual
          "parsing a Vector"
          "(0 (2 2 2 2) \"Anna\")"
          (show $ readExpr "#(0 (2 2 2 2) \"Anna\")")
          ,
          assertEqual
          "add two numbers"
          "49"
          (show $ readExpr "(+ 42 7)")
        ]
