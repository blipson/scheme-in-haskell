module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

data LispVal = Atom String
		| List [LispVal]
		| DottedList [LispVal] LispVal
		| Number Integer
		| String String
		| Bool Bool

hex2dig x = fst $ readHex x !! 0
oct2dig x = fst $ readOct x !! 0
bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in bin2dig' old xs

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
	return $ case atom of
		_    -> Atom atom

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

parseNumber :: Parser LispVal
parseNumber = do
	num <- parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin
	return $ num

parseExpr :: Parser LispVal
parseExpr = parseAtom
	<|> parseString
	<|> parseNumber
	<|> parseBool

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

