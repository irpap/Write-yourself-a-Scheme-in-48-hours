module Main where

import Data.Char
import Data.Complex
import Data.Ratio
import SchemeParser
import Test.HUnit

main = runTestTT $ test tests

tests :: Test
tests = TestList $ map TestCase [
    assertEqual "Parsing a complex number"
      (Complex (3.32 :+ 5.0))
      (readExpr "3.32+5i"),

    assertEqual "Parsing a complex number with a negative imaginary part"
      (Complex (430.0 :+ (-2.343)))
      (readExpr "430-2.343i"),

    assertEqual "Parsing a rational number"
      (Rational (3 % 1))
      (readExpr "6/2"),

    assertEqual "Parsing a character"
      (Char 'l')
      (readExpr "#\\l") ,

    assertEqual "Parsing a string"
      (String "lol")
      (readExpr "\"lol\""),

    assertEqual "Parsing a boolean"
      (Bool True)
      (readExpr "#t")
    ]
