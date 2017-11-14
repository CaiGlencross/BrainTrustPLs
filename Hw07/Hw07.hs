{-# LANGUAGE BangPatterns #-}

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

import System.Environment
import System.Exit
import System.IO
import System.Random

type VarName = String

--VarT and Var && LambdaT and Lambda should be consolidated okthxbye jk lol
data Expr = 
      VarT VarName Type
    | Var VarName
    | App Expr Expr
    | LambdaT VarName Type Expr
    | Lambda VarName Expr
    | Cond Expr Expr Expr
    | TypeDec Expr Type
    | Num Integer
    | LTrue
    | LFalse
    | LTuple Expr Expr
    | Unop Expr
    | Binop Expr Expr
    deriving (Eq, Ord)

data Unop = 
      Neg 
    | Not
    | Fst
    | Snd

data Binop = 
      Plus 
    | Times
    | Div
    | Minus
    | And
    | Or
    | Equal
    | Lt
    | Gt
    | Lte
    | Gte
 
data Type =
      Int
    | Boolean
    | Arrow Type Type
    | Tuple Type Type 
    deriving (Eq, Show, Ord)

data Error = 
     NotAChurchNumeral Expr
   | InvalidApplication Expr

instance Show Error where
    show (NotAChurchNumeral e) = "Not a valid church numeral" ++ show(e)
    show (InvalidApplication e) = "Can't apply things that are not functions" ++ show(e)

instance Show Expr where
    show (Var v) = v
    show (App x y) = "(" ++ show(x) ++ " " ++ show(y) ++ ")"
    show (Lambda var e) = "(lambda "++ var ++ ". " ++ show(e) ++ ")"
    show (LambdaT var t e) = "(lambda "++ var ++ " : " ++ show(t) ++ ". " ++ show(e) ++ ")"

interp :: Expr -> Expr 
interp (Var var)        = Var var
interp (Lambda var e)   = Lambda var e
interp (App e1 e2)      = 
    case interp e1 of
        (Lambda var' e')  -> let !e2' = interp e2 in interp (subst e' var' e2')
        (App a b)         -> let !e2' = interp e2 in App (App a b) e2'
        (Var a)           -> let !e2' = interp e2 in App (Var a) e2'

subst :: Expr -> VarName -> Expr -> Expr
subst (Var orig) var sub     = if orig == var then sub else (Var orig)
subst (App e1 e2) var sub    = App (subst e1 var sub) (subst e2 var sub)
subst (Lambda v e) var sub   = 
    case v == var of
        True -> (Lambda v e)
        False -> (Lambda v (subst e var sub))
subst e v s                  = error $"recieved wrong arguments" ++ show(e) ++v ++show(s)

isAlphaNum' a = isAlphaNum a || a == '\''

alphaNum' :: Parser String
alphaNum' = do
    result <- many1 $ satisfy isAlphaNum'
    return result

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
{-
lambda :: Parser Expr
lambda = do
    spaces    
    string "lambda"
    notFollowedBy alphaNum'
    args
-}

{-
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




--lambdaArg :: Parser Expr
--lambdaArg = 
--    try (do{
--        lam <- arg;
--        return lam;}) <|>
--    do {
--       let ar =(char '(') *> arg <* (do {spaces; char ')';});
--       char '.';} <|>
--    error "mismatched parens"

arg = try (do {
    arg <- varName;
    spaces;
    char ':';
    spaces;
    aType <- argType;
    spaces;
    char '.';
    body <- app;
    return (LambdaT arg aType body);}) <|>
    do {
    arg <- varName;
    spaces;
    char ':';
    spaces;
    aType <- argType;
    spaces;
    body <- app;
    return (LambdaT arg aType body);
    }
-}

lambda = do
    spaces
    string "lambda"
    notFollowedBy alphaNum'
    spaces;
    lam <- args
    return lam

args = try (do {
    partialLam <- arg;
    char '.';
    body <- app;
    return (partialLam body)}) <|>
    do
    partialLam <- arg
    recLambda <- args
    return (partialLam recLambda)

arg = try (do {
    char '(';
    partialLam <- nameType;
    char ')';
    spaces;
    return (partialLam);}) <|>
    do
    partialLam <- nameType
    spaces;
    return partialLam

nameType = do
    arg <- varName
    spaces;
    char ':';
    spaces;
    aType <- argType;
    spaces;
    return (LambdaT arg aType)

-- Need to add tuples
argType :: Parser Type
argType = try (do {
    word <- many1 letter;
    let {aType = getType word};
    spaces;
    string "->";
    spaces;
    recType <- argType;
    return (Arrow aType recType) }) <|> 
    do {
    word <- many1 letter;
    let {aType = getType word};
    spaces;
    return aType
    }

getType :: String -> Type
getType s =
    if s == "int" then Int
    else if s == "bool" then Boolean
    else error ("Not a valid type: " ++ s)

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
test p s = case parse p "" s of 
    (Right v) -> v
    (Left e) -> error (show e)

testInterp :: String -> Expr
testInterp s = case parse lambdaExpr "" s of 
    (Right v) -> (interp v); 
    (Left e) -> error (show e)

main :: IO ()
main = do
    args <- getArgs
    let cflag = any (\a -> a=="-c" || a=="-cn") args 
    let nflag = any (\a -> a=="-n" || a=="-cn") args
    let fileArg = Prelude.filter (\a -> a/="-c" && a/="-n" && a/="-cn") args
    
    if length fileArg > 1 then error "Usage error: too many args"
    else do
    input <- if head fileArg == "-" || length fileArg == 0 then getContents
             else readFile (head fileArg)
    let ast = case parse lambdaExpr "" input of 
                   (Right v) -> v 
                   (Left e) -> error (show e)
    let output = interp ast
{-
    if cflag && (unbounded ast) then 
        error "Unbound variables"
    else if nflag then do
        putStrLn (show (fullChurch output))
    else do-}
    putStrLn (show (output))
