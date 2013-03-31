import Test.HUnit
import Data.Char
import Data.Complex
import SchemeParser

testParsingComplex = TestCase $ assertEqual "tests parsing a complex number" (readExpr "3.32+5i") "Found value: Complex (3.32 :+ 5.0)"

