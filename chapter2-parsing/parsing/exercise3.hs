import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispVal = Atom String
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Number Integer
                | String String
                | Bool Bool
                
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseEscapedChar :: Parser Char
parseEscapedChar = char '\\' >> oneOf("\"nrt\\")

parseString :: Parser LispVal
parseString = do  
                  char '"'
                  x <- many (noneOf "\"" <|> parseEscapedChar)
                  char '"'
                  return $ String x

parseAtom :: Parser LispVal
parseAtom = do 
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest
                return $ case atom of
                            "#t" -> Bool True
                            "#f" -> Bool False
                            _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= \s -> return (Number (read s))

parseExpr :: Parser LispVal
parseExpr = parseAtom 
            <|> parseString
            <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err ->"No match: " ++ show err
    Right val -> "Found value"

main :: IO()
main = do 
    args <- getArgs
    putStrLn(readExpr $ args !! 0)
