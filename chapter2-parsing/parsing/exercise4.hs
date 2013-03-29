import Data.Char
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispVal = Atom String
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Number Integer
                | String String
                | Bool Bool
                | Char Char
                deriving (Eq, Show)
main :: IO()
main = do 
    args <- getArgs
    putStrLn(readExpr $ args !! 0)

parseExpr :: Parser LispVal
parseExpr = try parseNumber
            <|> parseAtom
            <|> parseString

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err ->"No match: " ++ show err
    Right val -> "Found value: " ++  show val

parseAtom :: Parser LispVal
parseAtom = do 
                first <- letter <|> symbol
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

escapedChar :: Parser Char
escapedChar = char '\\' >> oneOf("\"nrt\\") >>= \c -> 
                            return $ case c of
                                    '\\' -> '\\'
                                    'n' -> '\n'
                                    'r' -> '\r'
                                    't' -> '\t'

parseString :: Parser LispVal
parseString = do  
                  char '"'
                  x <- many (noneOf "\"" <|> escapedChar)
                  char '"'
                  return $ String x


parseNumber :: Parser LispVal
parseNumber = readPlainNumber <|> parseNumberInBase

readPlainNumber:: Parser LispVal
readPlainNumber = do
                    d <- many1 digit
                    return $ Number . read $ d

parseNumberInBase :: Parser LispVal
parseNumberInBase = char '#' >> 
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
                    return $ Number $ toDecimal d base

toDecimal :: String -> Integer -> Integer
toDecimal s base  = ( foldl1 ((+) . (* base)) ( map  (toInteger . digitToInt) s ))

