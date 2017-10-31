import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Token
import Text.Parsec.Combinator
import Text.Parsec
import Text.ParserCombinators.Parsec.Error

import Data.Char 
import Data.Map
import Data.Set

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
    deriving (Eq, Ord)

instance Show Expr where
    show (Var v) = v
    show (App x y) = "(" ++ show(x) ++ " " ++ show(y) ++ ")"
    show (Lambda var e) = "lambda "++ var ++ ". " ++ show(e)

-- bounded :: Set -> Expr -> Bool
bounded vars (Var name) = Data.Set.member name vars
bounded vars (App e1 e2) = (bounded vars e1) && (bounded vars e2)
bounded vars (Lambda arg e) = 
    let 
        vars' = Data.Set.insert arg vars 
    in
        bounded vars' e

-- need to know which ones were not bound?
unbounded expr = not (bounded Data.Set.empty expr) 

-- how to ensure there are not free variables?
-- Walk the tree checking that every var occurs as an arg?

isAlphaNum' a = isAlphaNum a || a == '\''

alphaNum' :: Parser String
alphaNum' = do
    result <- many1 $ satisfy isAlphaNum'
    return result

lambdaExpr = try lets <|> app

lets = do
    spaces
    string "let"
    notFollowedBy alphaNum'
    spaces
    name <- varName 
    spaces
    char '='
    spaces
    value <- app
    spaces
    string "in"
    spaces
    body <- lambdaExpr
    return (App (Lambda name body) value)

app = expr `chainl1` appOp

-- need some way of ensuring there is not an end of
-- file or input after the spaces

appOp = try (do {
    skipMany1 space;
    notFollowedBy (string "in");
    notFollowedBy eof;
    return (App);
    })

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

-- trailing whitespace bugs with
-- test lambdaExpr "lambda x. lambda y. lambda z. (x (y z)) "

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

test :: Parser a -> String -> a
test p s = case parse p "" s of (Right v) -> v; (Left e) -> error (show e)

main :: IO ()
main = do
    args <- getArgs
    let cflag = any (\a -> a=="-c" || a=="-cn") args 
    let nflag = any (\a -> a=="-n" || a=="-cn") args
    let fileArg = Prelude.filter (\a -> a=="-c" && a=="-n" && a=="-cn") args
    
    if length fileArg > 1 then error "Usage error: too many args"
    else do
    input <- if head fileArg == "-" || length fileArg == 0 then getContents
             else readFile (head fileArg)
    let output = case parse app "" input of 
                   (Right v) -> v
                   (Left e) -> error (show e)
    if cflag && (unbounded output) then 
        error "Unbound variables"
    else do
        putStrLn (show output)
