module Main where
import Control.Monad (liftM)
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

-- Exercise 1
parseNumber :: Parser LispVal

-- parseNumber = do
--   numStr <- many1 digit 
--   let numInt = read numStr
--   return $ Number numInt

parseNumber = many1 digit
  >>= 
  (\ numStr -> return $ Number $ read numStr)

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String 
readExpr input = case parse parseExpr "lisp" input of 
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

main :: IO ()
main = do 
  args <- getArgs
  putStrLn (readExpr (args !! 0))
