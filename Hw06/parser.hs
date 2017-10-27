import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Token
import Text.Parsec.Combinator
import Text.Parsec
import Text.ParserCombinators.Parsec.Error

import Data.Char 

import Control.Monad
-- import Control.Applicative

import System.Environment
import System.Exit
import System.IO
import System.Random

type VarName = String

data Expr = 
      Var VarName
    | App Expr Expr
    | Lambda VarName Expr
    deriving (Show, Eq, Ord)

-- make a pretty printer of show to test output on input and make
-- sure it's parsed to the same
--
-- how to ensure there are not free variables?
-- Walk the tree checking that every var occurs as an arg?

testParser :: Parser a -> String -> a
testParser p s = case parse p "" s of (Right v) -> v; (Left e) -> error (show e)

isAlphaNum' a = isAlphaNum a || a == '\''

alphaNum' :: Parser String
alphaNum' = do
    result <- many1 $ satisfy isAlphaNum'
    return result

app = expr `chainl1` appOp
appOp = do {char ' '; return (App)}

expr :: Parser Expr
expr = 
    try lambda <|>
    var

lambda :: Parser Expr
lambda = do
    spaces    
    string "lambda"
    notFollowedBy alphaNum'
    args
    

args :: Parser Expr
args = try (do {
    spaces;
    arg <- varName;
    spaces;
    char '.';
    spaces;
    body <- app;
    return (Lambda arg body)}) <|>
    do {
    spaces;
    arg <- varName;
    spaces;
    nextArg <- args;
    return (Lambda arg nextArg)
    }



var :: Parser Expr
var = do {
    name <- varName;
    return (Var name)} <|>
    between (char '(') (char ')') app

varName :: Parser VarName
varName = do
    spaces
    fc <- firstChar
    rest <- many nonFirstChar
    return (fc:rest)
  where
    firstChar = satisfy (\a -> isLetter a)
    nonFirstChar = satisfy isAlphaNum' 

main :: IO ()
main = do
    arg <- getLine
    putStrLn arg
