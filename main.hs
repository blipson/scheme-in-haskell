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

hex2dig x = fst $ readHex x !! 0
oct2dig x = fst $ readOct x !! 0
bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in bin2dig' old xs

toDouble :: LispVal -> Double
toDouble(Float f) = realToFrac f
toDouble(Number n) = fromIntegral n

escapedChars :: Parser String
escapedChars = do
	char '\\'
	c <- oneOf "\\\"ntr"
	case c of
		'\\' -> do return [c]
		'"' -> do return [c]
		't' -> do return "\t"
		'n' -> do return "\n"
		'r' -> do return "\r"

parseRatio :: Parser LispVal
parseRatio = do
	x <- many1 digit
	char '/'
	y <- many1 digit
	return $ Ratio ((read x) % (read y))

parseChar :: Parser LispVal
parseChar = do
	try $ string "#\\"
	c <- many1 letter
	return $ case (map toLower c) of
		"space" -> Char ' '
		"newline" -> Char '\n'
		[x] -> Char x

parseString :: Parser LispVal
parseString = do
	char '"'
	x <- many $ many1 (noneOf "\"\\") <|> escapedChars
	char '"'
	return $ String (concat x)

parseAtom :: Parser LispVal
parseAtom = do
	first <- letter <|> symbol
	rest <- many (letter <|> digit <|> symbol)
	let atom = first:rest
	return $ Atom atom	

parseBool :: Parser LispVal
parseBool = do
	string "#"
	c <- oneOf "tf"
	return $ case c of
		't' -> Bool True
		'f' -> Bool False

parseDigital1 :: Parser LispVal
parseDigital1 = do
	c <- many1 digit
	(return . Number . read) c

parseDigital2 :: Parser LispVal
parseDigital2 = do
	try $ string "#d"
	c <- many1 digit
	(return . Number . read) c

parseHex :: Parser LispVal
parseHex = do
	try $ string "#x"
	c <- many1 hexDigit
	return $ Number (hex2dig c)

parseOct :: Parser LispVal
parseOct = do
	try $ string "#o"
	c <- many1 octDigit
	return $ Number (oct2dig c)

parseBin :: Parser LispVal
parseBin = do
	try $ string "#b"
	c <- many1 (oneOf "10")
	return $ Number (bin2dig c)

parseFloat :: Parser LispVal
parseFloat = do
	whole <- many1 digit
	char '.'
	decimal <- many1 digit
	return $ Float (read (whole++"."++decimal) :: Double)

parseNumber :: Parser LispVal
parseNumber = do
	num <- parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin
	return $ num

parseComplex :: Parser LispVal
parseComplex = do
	x <- (try parseFloat <|> parseDigital1)
	char '+'
	y <- (try parseFloat <|> parseDigital1)
	char 'i'
	return $ Complex (toDouble x :+ toDouble y)

parseVector :: Parser LispVal
parseVector = do
	arrayValues <- sepBy parseExpr spaces
	return $ Vector (listArray (0,(length arrayValues - 1)) arrayValues)

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

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
	head <- endBy parseExpr spaces
	tail <- char '.' >> spaces >> parseExpr
	return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
	char '\''
	x <- parseExpr
	return $ List [Atom "quote", x]

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
	Left err -> "No match: " ++ show err
	Right val -> "Found value"

main :: IO ()
main = do
	(expr:_) <- getArgs
	putStrLn (readExpr expr)

