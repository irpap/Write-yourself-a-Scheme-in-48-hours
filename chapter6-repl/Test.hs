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
          "Parsing a boolean"
          (Right $ Bool True)
          (readExpr "#t" >>= eval)
          ,
          assertEqual
          "list"
          (Right $ List [ List [Number 1,Number 2,Number 3], List [Number 4, Number 5]])
          (readExpr "((1 2 3) (4 5))" >>= eval)
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
           ,
          assertEqual
          "Numeric comparison"
          (Right $ Bool False)
          (readExpr "(> 2 3)" >>= eval)
          ,
          assertEqual
          "String equality check"
          (Right $ Bool True)
          (readExpr "(string=? \"test\"  \"test\")" >>= eval)
          ,
          assertEqual
          "String lexicographic comparison"
          (Right $ Bool True)
          (readExpr "(string<? \"Astronaut\"  \"Avocado\")" >>= eval)
          ,
          assertEqual
          "If then else"
          (Right $ String "yes")
          (readExpr "(if (> 2 3) \"no\" \"yes\")" >>= eval)
          ,
          assertEqual
          "car"
          (Right $ Atom "rose")
          (readExpr "(car '(rose violet daisy buttercup))"  >>= eval)
          ,
          assertEqual
          "cdr"
          (Right $ List [Atom "violet", Atom "daisy", Atom "buttercup"])
          (readExpr "(cdr '(rose violet daisy buttercup))"  >>= eval)
          ,
          assertEqual
          "eqv?"
          (Right $ Bool True)
          (readExpr "(eqv? \"lol\" \"lol\")"  >>= eval)
          ,
          assertEqual
          "cons"
          (Right $ List [Atom "pine", Atom "fir", Atom "oak", Atom "maple"])
          (readExpr "(cons 'pine '(fir oak maple))" >>= eval)
          ,
          assertEqual
          "equal?"
          (Right $ Bool True)
          (readExpr "(equal? \"2\" 2)" >>= eval)
          ,
          assertEqual
          "cond"
          (Right $ Number 3)
          (readExpr ("(cond ( (equal? 1 2) 5) (#t 3) )") >>= eval)
          ,
          assertEqual
          "case"
          (Right $ Atom "composite")
          (readExpr "(case (* 2 3)\
                    \ ((2 3 5 7) 'prime)\
                    \ ((1 4 6 8 9) 'composite))" >>= eval)
        ]
