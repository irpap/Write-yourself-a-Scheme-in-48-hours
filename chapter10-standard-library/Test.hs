module Main where

import Control.Monad
import Data.Char
import Data.Complex
import Data.Text.Unsafe
import SchemeParser
import Test.HUnit

main = runTestTT $ test tests

getValue a = inlinePerformIO $ primitiveBindings >>= \env -> runIOThrows $ liftM show $ liftThrows (readExpr a) >>= eval env

tests :: Test
tests = TestList $ map TestCase [
          assertEqual
          "Parsing a number"
          "34"
          (getValue "34")
          ,
          assertEqual
          "Parsing a boolean"
          "#t"
          (getValue "#t")
          ,
          assertEqual
          "list"
          "((1 2 3) (4 5))"
          (getValue "((1 2 3) (4 5))")
          ,
          assertEqual
          "Type error when + is applied to string"
          "Invalid type: expected number, found \"two\""
          (getValue "(+ 2 \"two\")" )
           ,
          assertEqual
          "Type error when + is applied to string"
          "Expected 2 args; found values 2"
          (getValue "(+ 2 )")
           ,
          assertEqual
          "Numeric comparison"
          "#f"
          (getValue "(> 2 3)")
          ,
          assertEqual
          "String equality check"
          "#t"
          (getValue "(string=? \"test\"  \"test\")")
          ,
          assertEqual
          "String lexicographic comparison"
          "#t"
          (getValue "(string<? \"Astronaut\"  \"Avocado\")")
          ,
          assertEqual
          "If then else"
          "\"yes\""
          (getValue "(if (> 2 3) \"no\" \"yes\")")
          ,
          assertEqual
          "car"
          "rose"
          (getValue "(car '(rose violet daisy buttercup))")
          ,
          assertEqual
          "cdr"
          "(violet daisy buttercup)"
          (getValue "(cdr '(rose violet daisy buttercup))")
          ,
          assertEqual
          "eqv?"
          "#t"
          (getValue "(eqv? \"lol\" \"lol\")")
          ,
          assertEqual
          "cons"
          "(pine fir oak maple)"
          (getValue "(cons 'pine '(fir oak maple))")
          ,
          assertEqual
          "equal?"
          "#t"
          (getValue "(equal? \"2\" 2)")
          ,
          assertEqual
          "cond"
          "3"
          (getValue "(cond ( (equal? 1 2) 5) (#t 3) )")
          ,
          assertEqual
          "case"
          "composite"
          (getValue "(case (* 2 3)\
                    \ ((2 3 5 7) 'prime)\
                    \ ((1 4 6 8 9) 'composite))")
        ]
