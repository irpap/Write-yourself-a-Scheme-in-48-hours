import Data.Char
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric

data LispVal = Atom String
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Number Integer
                | Float Double
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
            <|> try  parseChar
            <|> try parseFloat
            <|> parseAtom
            <|> parseString

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err ->"No match: " ++ show err
    Right val -> "Found value: " ++  show val

parseFloat :: Parser LispVal
parseFloat = do whole <- many digit
                char '.'
                decimal <- many digit
                return $ Float $ fst $ head $ (readFloat (whole ++ "." ++ decimal)) 

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

