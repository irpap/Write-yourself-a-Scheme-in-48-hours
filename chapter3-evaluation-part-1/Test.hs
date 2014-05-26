module Main where

import Data.Char
import Data.Complex
import SchemeParser
import Test.HUnit

main = runTestTT $ test tests

tests :: Test
tests = TestList $ map TestCase [
          assertEqual
          "Parsing a number"
          "33"
          (show $ readExpr "33")
          ,
          assertEqual
          "Parsing a complex number"
          "3.32 :+ 5.0"
          (show $ readExpr "3.32+5i")
          ,
          assertEqual
          "Parsing a rational number"
          "3 % 1"
          (show $ readExpr "6/2")
          ,
          assertEqual
          "Parsing a list"
          "(1 2 3)"
           (show $ readExpr "(1 2 3)")
          ,
          assertEqual
          "Parsing a dotted list"
          "(1.(2.(3.nil)))"
          (show $ readExpr "(1 . (2 . (3 . nil)))")
          ,
          --assertEqual
          --"Parsing a quasiquoted expr"
          --"(quasiquote (1 2 (unquote (+ 3 4))))"
          --(show $ readExpr "`(1 2 ,(+ 3 4))")
          --,
          assertEqual
          "Parsing a Vector"
          "(0 (2 2 2 2) \"Anna\")"
          (show $ readExpr "#(0 (2 2 2 2) \"Anna\")")
          ,
          assertEqual
          "add two numbers"
          "46"
          (show $ readExpr "(+ 42 (- 7 3))")
          ,
          assertEqual
          "add three numbers, some quoted"
          "34"
          (show $ readExpr "(+ \"23\" 7 \"4\")")
          ,
          assertEqual
          "Non numeric strigs are treated as zero"
          "7"
          (show $ readExpr "(+ \"haha\" 7)")
          ,
          assertEqual
          "Symbol? recognises a symbol"
          "#t"
          (show $ readExpr "(symbol? 'lol)")
          ,
          assertEqual
          "Symbol? recognises a non symbol"
          "#f"
          (show $ readExpr "(symbol? (1 2 3))")
        ]
