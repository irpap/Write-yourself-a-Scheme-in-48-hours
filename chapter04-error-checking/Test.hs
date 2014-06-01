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
          (Right (Number 33))
          (readExpr "33" >>=eval)
          ,
          assertEqual
          "Type error when + is applied to string"
          (Left $ TypeMismatch "number" $ String "two")
          (readExpr "(+ 2 \"two\")" >>= eval)
           ,
          assertEqual
          "Type error when + is applied to string"
          (Left $ NumArgs 2 $ [Number 2])
          (readExpr "(+ 2 )" >>= eval)
        ]
