{-# LANGUAGE ExistentialQuantification #-}

module SchemeParser (LispVal (..), LispError (..), readExpr, eval, runIOThrows, primitiveBindings, liftThrows) where

import Control.Monad
import Control.Monad.Error
import Data.Array
import Data.Char
import Data.Complex
import Data.IORef
import Data.Maybe
import Data.Ratio
import Numeric
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces1)

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
                | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
                | Func { params :: [String], vararg :: Maybe String,
                    body :: [LispVal], closure :: Env }
                | IOFunc ([LispVal] -> IOThrowsError LispVal)
                | Port Handle

instance Show LispVal where show = showVal
instance Eq LispVal where a == b = a `eq` b
eq :: LispVal -> LispVal -> Bool
eq (Atom a) (Atom b) = a == b
eq (List a) (List b) = a == b
eq (DottedList a b)(DottedList c d) = a == c && b == d
eq (Vector a) (Vector b) = a==b
eq (Number a) (Number b) = a==b
eq (Float a) (Float b) = a==b
eq (Complex a) (Complex b) = a==b
eq (Rational a) (Rational b) = a==b
eq (String a) (String b) = a==b
eq (Bool a) (Bool b) = a==b
eq (Char a) (Char b) = a==b
eq _ _ = False

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError
instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default
instance Eq LispError where a == b = a `eqError` b

eqError (NumArgs a b) (NumArgs c d) = (a == c) && (b == d)
eqError (TypeMismatch a b) (TypeMismatch c d) = (a == c) && (b == d)
eqError (BadSpecialForm a b) (BadSpecialForm c d) = (a == c) && (b == d)
eqError (NotFunction a b) (NotFunction c d) = (a == c) && (b == d)
eqError (UnboundVar a b) (UnboundVar c d) = (a == c) && (b == d)
eqError (Default a) (Default b) = a == b
eqError a b  = False

type ThrowsError = Either LispError

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ErrorT LispError IO

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = liftM extractValue (runErrorT (trapError action))

isBound :: Env -> String -> IO Bool
isBound envRef var = liftM (isJust . lookup var) (readIORef envRef)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . flip writeIORef value)
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   unless (pred result) $ action result >> until_ pred prompt action

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map (makeFunc IOFunc) ioPrimitives ++
                                               map (makeFunc PrimitiveFunc) primitives)
     where makeFunc constructor (var, func) = (var, constructor func)

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    runIOThrows (liftM show $ eval env (List [Atom "load", String (head args)]))
        >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne args

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

trapError action = catchError action (return . show)

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _)   = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default message)             = "Error: " ++ message

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

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "load", String filename]) =
     load filename >>= liftM last . mapM (eval env)
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt ]) = do
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    Bool True -> eval env conseq
    otherwise -> throwError $ BadSpecialForm "Predicate must be boolean in" otherwise
eval env (List (Atom "cond" : (h@(List [test, expr]) : clauses))) = do
  result <- eval env test
  case result of
    Bool True -> eval env expr
    Bool False -> eval env (List (Atom "cond" : clauses))
    pred -> throwError $ TypeMismatch "boolean" pred
eval env (l@(List (Atom "cond": []))) = throwError $ BadSpecialForm "One of the conditions must be true" l
eval env (List (Atom "cond": a)) = throwError $ TypeMismatch "list" (head a)
eval env form@(List (Atom "case" : key : clauses)) =
  if null clauses
  then throwError $ BadSpecialForm "no true clause in case expression: " form
  else case head clauses of
    List (Atom "else" : exprs) -> liftM last (mapM (eval env) exprs)
    List (List datums : exprs) -> do
      result <- eval env key
      equality <-  liftThrows (mapM (\x -> eqv [result, x]) datums)
      if Bool True `elem` equality
        then liftM last (mapM (eval env) exprs)
        else eval env $ List (Atom "case" : key : tail clauses)
    _  -> throwError $ BadSpecialForm "ill-formed case expression: " form
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
   makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
   makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
   makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
   makeVarArgs varargs env [] body
eval env (List (function : args)) = do
   func <- eval env function
   argVals <- mapM (eval env) args
   apply func argVals
eval env (List elems) = return $ List elems
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
      if num params /= num args && isNothing varargs
         then throwError $ NumArgs (num params) args
         else liftIO (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            num = toInteger . length
            evalBody env = liftM last $ mapM (eval env) body
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
                Nothing -> return env
apply (IOFunc func) args = func args
apply a bs = return $ List (a:bs)

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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
              ("string->symbol", unaryOp string2symbol),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)
              ]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

listOp :: ([LispVal] -> ThrowsError LispVal) ->[LispVal] -> ThrowsError LispVal
listOp op = op

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = liftM (Number . foldl1 op) (mapM unpackNum params)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum


unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

parseVector :: Parser LispVal
parseVector = do string "#("
                 elems <- sepBy parseExpr spaces1
                 char ')'
                 return $ Vector (listArray (0, length elems -1) elems)

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = f v

symbolp, numberp, stringp, boolp, listp, string2symbol, symbol2string :: LispVal -> ThrowsError LispVal
symbolp (Atom _) = return $ Bool True
symbolp _ = return $ Bool False

numberp (Number _) = return $ Bool True
numberp _          = return $ Bool False

stringp (String _) = return $ Bool True
stringp _          = return $ Bool False

boolp   (Bool _)   = return  $ Bool True
boolp   _          = return $ Bool False

listp   (List _)   = return $ Bool True
listp   (DottedList _ _) = return $ Bool True
listp   _          = return $ Bool False

string2symbol (String x) = return $ Atom x
string2symbol s = throwError $ TypeMismatch "string" s

symbol2string (Atom x) = return $ String x
symbol2string s = throwError $ TypeMismatch "symbol" s

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ head args
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2]             = return $ Bool $ arg1 == arg2
eqv [Number arg1,Number arg2]         = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2]         = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2]             = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [l1@(List arg1), l2@(List arg2)]       = eqvList eqv [l1, l2]
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [List arg1, List arg2] = return $ Bool $ (length arg1 == length arg2) &&
                                                  all eqvPair (zip arg1 arg2)
   where eqvPair (x1, x2) = case eqvFunc [x1, x2] of
                                 Left err -> False
                                 Right (Bool val) -> val

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [l1@(List arg1), l2@(List arg2)] = eqvList equal [l1, l2]
equal [DottedList xs x, DottedList ys y] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
   primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                      [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
   eqvEquals <- eqv [arg1, arg2]
   return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList



parseAllTheLists ::Parser LispVal
parseAllTheLists = do char '(' >> spaces
                      head <- sepEndBy parseExpr spaces1
                      do  char '.' >> spaces1
                          tail <- parseExpr
                          spaces >> char ')'
                          return $ DottedList head tail
                          <|> (spaces >> char ')' >> return (List head))

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
parseComplexNumber = do realPart <- fmap toDouble $ try parseFloat <|> readPlainNumber
                        sign <- char '+' <|> char '-'
                        imaginaryPart <- fmap toDouble $ try parseFloat <|> readPlainNumber
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
escapedChar = char '\\' >> oneOf "\"nrt\\" >>= \c ->
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
                    d <- many (oneOf digits)
                    return $ Number $ toDecimal base d

toDecimal :: Integer -> String -> Integer
toDecimal base s = foldl1 ((+) . (* base)) $ map toNumber s
                    where toNumber  = toInteger . digitToInt

