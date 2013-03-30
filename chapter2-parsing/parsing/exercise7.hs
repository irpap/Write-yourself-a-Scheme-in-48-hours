import Data.Char
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric
import Data.Ratio

type Complex = (Integer, Integer)
data LispVal = Atom String
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Number Integer
                | Float Double
                | Complex Complex
                | Rational Rational
                | String String
                | Bool Bool
                | Char Char
                deriving (Eq, Show)
main :: IO()
main = do 
    args <- getArgs
    putStrLn(readExpr $ args !! 0)

parseExpr :: Parser LispVal
parseExpr = try parseFloat
            <|> try parseComplexNumber
            <|> try parseRationalNumber
            <|> try parseNumber
            <|> try parseChar
            <|> parseAtom
            <|> parseString

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err ->"No match: " ++ show err
    Right val -> "Found value: " ++  show val

parseComplexNumber :: Parser LispVal
parseComplexNumber = do realPart <- many digit
                        char '+'
                        imaginaryPart <- many digit
                        char 'i'
                        return $ Complex (read realPart :: Integer , read imaginaryPart :: Integer)

parseRationalNumber :: Parser LispVal
parseRationalNumber = do numerator <- many digit
                         char '/'
                         denominator <- many digit
                         return $ Rational (read (numerator ++ "%" ++ denominator) :: Rational)
                    
parseFloat :: Parser LispVal
parseFloat = do whole <- many digit
                char '.'
                decimal <- many digit
                return $ Float (read (whole ++ "." ++ decimal) :: Double)

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                           "#t" -> Bool True
                           "#f" -> Bool False
                           _    -> Atom atom
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

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
