module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String 
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer 
  | String String 
  | Bool Bool
  deriving (Show)

parseString :: Parser LispVal 
parseString = do 
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do 
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = [first] ++ rest
  return $ case atom of 
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

-- Exercise 1a
-- parseNumber implementation with do
parseNumberDo :: Parser LispVal
parseNumberDo = do
  numStr <- many1 digit 
  let numInt = read numStr
  return $ Number numInt

-- Exercise 1b
-- parseNumber implementation with bind (>>=)
parseNumberBind :: Parser LispVal
parseNumberBind = many1 digit
  >>= 
  (\ numStr -> return $ Number $ read numStr)

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumberBind

readExpr :: String -> String 
readExpr input = case parse parseExpr "lisp" input of 
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

main :: IO ()
main = do 
  args <- getArgs
  putStrLn (readExpr (args !! 0))
