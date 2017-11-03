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


interp :: Expr -> Expr 
interp (Var var)        = Var var
interp (Lambda var e)   = Lambda var (interp e)
interp (App e1 e2)      = 
    case interp e1 of
        (Lambda var' e')   -> 
            let
                e2' = interp e2
            in
                subst e' var' e2'
        otherwise          -> error "the fuck happening?"




subst :: Expr -> VarName -> Expr -> Expr
subst (Var orig) var sub     =      if orig == var then sub else (Var orig)
subst (App e1 e2) var sub    = App (subst e1 var sub) (subst e2 var sub)
subst (Lambda v e) var sub   = 
    case v == var of
        True -> (Lambda v e)
        False -> (Lambda v (subst e var sub))





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

-- the entry point parser for all lambda expressions
lambdaExpr = do 
    results <- try lets <|> app
    notFollowedBy (char ')')
    return results

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

appOp = try (do {
    skipMany1 space;
    notFollowedBy (string "in");
    notFollowedBy eof;
    lookAhead (try (many1 anyChar));
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

-- mismatched parens not detected

var :: Parser Expr
var = do {
    name <- varName;
    return (Var name)} <|>
    between (char '(') (do {spaces; char ')'}) app <|>
    error "mismatched parens"

keywords = ["lambda", "let", "in"]

varName :: Parser VarName
varName = do
    spaces
    fc <- firstChar
    rest <- many nonFirstChar
    if fc:rest `elem` keywords then error "Keyword used as variable name"
    else do
    return (fc:rest)
  where
    firstChar = satisfy (\a -> isLetter a)
    nonFirstChar = satisfy isAlphaNum' 

test :: Parser a -> String -> a
test p s = case parse p "" s of (Right v) -> v; (Left e) -> error (show e)

testInterp :: Parser Expr -> String -> Expr
testInterp p s = case parse p "" s of (Right v) -> (interp v); (Left e) -> error (show e)

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
    let output = case parse lambdaExpr "" input of 
                   (Right v) -> v 
                   (Left e) -> error (show e)
    if cflag && (unbounded output) then 
        error "Unbound variables"
    else do

        putStrLn (show output)
