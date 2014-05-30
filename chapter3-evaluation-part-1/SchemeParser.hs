module SchemeParser (LispVal (..), readExpr) where

import Data.Char
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces1)
import Numeric
import Data.Array
import Data.Complex
import Data.Ratio
import System.Environment

data LispVal = Atom String
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Vector (Array Int LispVal)
                | Number Integer
                | Float Double
                | Complex  (Complex Double)
                | Rational Rational
                | String String
                | Bool Bool
                | Char Char
                deriving Eq

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Complex c) = show c
showVal (Float f) = show f
showVal (Rational r) = show r
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (Vector arr) = "(" ++ unwordsList (elems arr) ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

parseExpr :: Parser LispVal
parseExpr =  try parseBool
          <|> parseString
          <|> parseVector
          <|> parseAtom
          <|> parseChar
          <|> try parseComplexNumber
          <|> try parseFloat
          <|> try parseRationalNumber
          <|> parseNumber
          <|> parseQuoted
          <|> parseQuasiQuoted
          <|> parseUnQuote
          <|> parseAllTheLists

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> eval val

main = getArgs >>= print . readExpr . head


eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval a = a

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp symbolp),
              ("string?" , unaryOp stringp),
              ("number?" , unaryOp numberp),
              ("bool?", unaryOp boolp),
              ("list?" , unaryOp listp),
              ("symbol->string", unaryOp symbol2string),
              ("string->symbol", unaryOp string2symbol)
              ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop f args = Number $ foldl1 f (unpackList args)


unpackNumber :: LispVal -> Integer
unpackNumber (Number a) = a
unpackNumber _ = 0

unpackList :: [LispVal] -> [Integer]
unpackList = map unpackNumber

parseVector :: Parser LispVal
parseVector = do string "#("
                 elems <- sepBy parseExpr spaces1
                 char ')'
                 return $ Vector (listArray (0, (length elems)-1) elems)

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

symbolp, numberp, stringp, boolp, listp, string2symbol, symbol2string :: LispVal -> LispVal
symbolp (Atom _) = Bool True
symbolp _ = Bool False

numberp (Number _) = Bool True
numberp _          = Bool False

stringp (String _) = Bool True
stringp _          = Bool False

boolp   (Bool _)   = Bool True
boolp   _          = Bool False

listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool True
listp   _          = Bool False

string2symbol (String x) = Atom x
string2symbol _ = Atom ""

symbol2string (Atom x) = String x
symbol2string _ = String ""

parseAllTheLists ::Parser LispVal
parseAllTheLists = do char '(' >> spaces
                      head <- sepEndBy parseExpr spaces1
                      do  char '.' >> spaces1
                          tail <- parseExpr
                          spaces >> char ')'
                          return $ DottedList head tail
                          <|> (spaces >> char ')' >> (return $ List head))

parseQuoted :: Parser LispVal
parseQuoted = do
        char '\''
        x <- parseExpr
        return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
   char '`'
   x <- parseExpr
   return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
   char ','
   x <- parseExpr
   return $ List [Atom "unquote", x]

parseComplexNumber :: Parser LispVal
parseComplexNumber = do realPart <- fmap toDouble $ (try parseFloat) <|> readPlainNumber
                        sign <- char '+' <|> char '-'
                        imaginaryPart <- fmap toDouble $ (try parseFloat) <|> readPlainNumber
                        let signedImaginaryPart = case sign of
                                                    '+' -> imaginaryPart
                                                    '-' -> negate imaginaryPart
                        char 'i'
                        return $ Complex (realPart :+ signedImaginaryPart)
                            where toDouble (Float x) = x
                                  toDouble (Number x) = fromInteger x :: Double

parseRationalNumber :: Parser LispVal
parseRationalNumber = do numerator <- many digit
                         char '/'
                         denominator <- many digit
                         return $ Rational (read (numerator ++ "%" ++ denominator) :: Rational)

parseFloat :: Parser LispVal
parseFloat = do whole <- many1 digit
                char '.'
                decimal <- many1 digit
                return $ Float (read (whole ++ "." ++ decimal))

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ Atom atom

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space

parseChar :: Parser LispVal
parseChar = do string "#\\"
               c <- many1 letter
               return $ case map toLower c of
                   "newline" -> Char '\n'
                   "space" -> Char ' '
                   [x] -> Char x

escapedChar :: Parser Char
escapedChar = char '\\' >> oneOf("\"nrt\\") >>= \c ->
                            return $ case c of
                                    '\\' -> '\\'
                                    'n' -> '\n'
                                    'r' -> '\r'
                                    't' -> '\t'

parseString :: Parser LispVal
parseString = do  char '"'
                  x <- many (noneOf "\"" <|> escapedChar)
                  char '"'
                  return $ String x

parseNumber :: Parser LispVal
parseNumber = readPlainNumber <|> parseRadixNumber

readPlainNumber:: Parser LispVal
readPlainNumber = do
                    d <- many1 digit
                    return $ Number . read $ d

parseRadixNumber :: Parser LispVal
parseRadixNumber = char '#' >>
                    ((char 'd' >> readPlainNumber)
                     <|> (char 'b' >> readBinaryNumber)
                     <|> (char 'o' >> readOctalNumber)
                     <|> (char 'x' >> readHexNumber))

readBinaryNumber = readNumberInBase "01" 2
readOctalNumber = readNumberInBase "01234567" 8
readHexNumber = readNumberInBase "0123456789abcdefABCEDF" 16

readNumberInBase :: String -> Integer -> Parser LispVal
readNumberInBase digits base = do
                    d <- many (oneOf (digits))
                    return $ Number $ toDecimal base d

toDecimal :: Integer -> String -> Integer
toDecimal base s = foldl1 ((+) . (* base)) $ map toNumber s
                    where toNumber  =  (toInteger . digitToInt)

