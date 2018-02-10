module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad (liftM)
import Numeric (readOct, readHex)
import Data.Char (toLower)
import Data.Ratio
import Data.Complex
import Data.Array

data LispVal = Atom String
		| List [LispVal]
		| DottedList [LispVal] LispVal
		| Number Integer
		| String String
		| Bool Bool
		| Char Char
		| Float Double
		| Ratio Rational
		| Complex (Complex Double)
		| Vector (Array Int LispVal)

instance Show LispVal where show = showVal

hex2dig x = fst $ readHex x !! 0
oct2dig x = fst $ readOct x !! 0
bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in bin2dig' old xs

toDouble :: LispVal -> Double
toDouble(Float f) = realToFrac f
toDouble(Number n) = fromIntegral n

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
			  ("-", numericBinop (-)),
			  ("*", numericBinop (*)),
			  ("/", numericBinop div),
			  ("mod", numericBinop mod),
			  ("quotient", numericBinop quot),
			  ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
							if null parsed
								then 0
								else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

escapedChars :: Parser String
escapedChars = do
	char '\\'
	x <- oneOf "\\\"ntr"
	case x of
		'\\' -> do return [x]
		'"' -> do return [x]
		't' -> do return "\t"
		'n' -> do return "\n"
		'r' -> do return "\r"

parseAtom :: Parser LispVal
parseAtom = do
	first <- letter <|> symbol
	rest <- many (letter <|> digit <|> symbol)
	let atom = first:rest
	return $ Atom atom

parseBin :: Parser LispVal
parseBin = do
	try $ string "#b"
	x <- many1 (oneOf "10")
	return $ Number (bin2dig x)

parseBool :: Parser LispVal
parseBool = do
	string "#"
	x <- oneOf "tf"
	return $ case x of
		't' -> Bool True
		'f' -> Bool False

parseChar :: Parser LispVal
parseChar = do
	try $ string "#\\"
	x <- many1 letter
	return $ case (map toLower x) of
		"space" -> Char ' '
		"newline" -> Char '\n'
		[x] -> Char x

parseComplex :: Parser LispVal
parseComplex = do
	x <- (try parseFloat <|> parseDigital1)
	char '+'
	y <- (try parseFloat <|> parseDigital1)
	char 'i'
	return $ Complex (toDouble x :+ toDouble y)

parseDigital1 :: Parser LispVal
parseDigital1 = do
	x <- many1 digit
	(return . Number . read) x

parseDigital2 :: Parser LispVal
parseDigital2 = do
	try $ string "#d"
	x <- many1 digit
	(return . Number . read) x

parseDottedList :: Parser LispVal
parseDottedList = do
	head <- endBy parseExpr spaces
	tail <- char '.' >> spaces >> parseExpr
	return $ DottedList head tail

parseFloat :: Parser LispVal
parseFloat = do
	whole <- many1 digit
	char '.'
	decimal <- many1 digit
	return $ Float (read (whole++"."++decimal) :: Double)

parseHex :: Parser LispVal
parseHex = do
	try $ string "#x"
	x <- many1 hexDigit
	return $ Number (hex2dig x)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseNumber :: Parser LispVal
parseNumber = do
	x <- parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin
	return $ x

parseOct :: Parser LispVal
parseOct = do
	try $ string "#o"
	x <- many1 octDigit
	return $ Number (oct2dig x)

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
	char '`'
	x <- parseExpr
	return $ List [Atom "quasiquote", x]

parseQuoted :: Parser LispVal
parseQuoted = do
	char '\''
	x <- parseExpr
	return $ List [Atom "quote", x]

parseRatio :: Parser LispVal
parseRatio = do
	x <- many1 digit
	char '/'
	y <- many1 digit
	return $ Ratio ((read x) % (read y))

parseString :: Parser LispVal
parseString = do
	char '"'
	x <- many $ many1 (noneOf "\"\\") <|> escapedChars
	char '"'
	return $ String (concat x)

parseUnQuote :: Parser LispVal
parseUnQuote = do
	char ','
	x <- parseExpr
	return $ List [Atom "unquote", x]

parseVector :: Parser LispVal
parseVector = do
	arrayValues <- sepBy parseExpr spaces
	return $ Vector (listArray (0,(length arrayValues - 1)) arrayValues)

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

parseExpr :: Parser LispVal
parseExpr = parseAtom
	<|> parseString
	<|> try parseComplex
	<|> try parseFloat
	<|> try parseRatio
	<|> try parseNumber
	<|> try parseBool
	<|> try parseChar
	<|> parseQuoted
	<|> parseQuasiQuoted
	<|> parseUnQuote
	<|> try (do
			string "#("
			x <- parseVector
			char ')'
			return x)
	<|> do
		char '('
		x <- try parseList <|> parseDottedList
		char ')'
		return x

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

