{-# LANGUAGE BangPatterns #-}

import System.IO
import Control.Monad
import Data.Char

import Text.Parsec.Number
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Map (Map)
import qualified Data.Map as Map

type VarName = String

data Expr = 
      Var VarName
    | App Expr Expr
    | Lambda VarName Type Expr
    | Let VarName Expr Expr
    | If Expr Expr Expr
    | TypeDec Expr Type
    | Num Integer
    | Bool Bool
    | Tuple Expr Expr
    | ExprUnOp UnOp Expr
    | ExprBinOp BinOp Expr Expr
    deriving (Eq, Ord, Show)

data UnOp = 
      Neg 
    | Not
    | Fst
    | Snd
    deriving (Eq, Ord, Show)

data BinOp = 
      Plus 
    | Times
    | Div
    | Minus
    | And
    | Or
    | Equal
    | NotEqual
    | Lt
    | Gt
    | Lte
    | Gte
    deriving (Eq, Ord, Show)
 
data Type =
      Int
    | Boolean
    | Arrow Type Type
    | TupleType Type Type 
    deriving (Eq, Ord, Show)

data Error =
      TypeMismatch Expr
    | UnboundVariable Expr
    | FunctionEquivalence Expr
    deriving (Eq)


instance Show Error where
     show (TypeMismatch e) = "types do not match up in expression: " ++ show(e)
     show (UnboundVariable e) = "there is an unbound variable in expression: " ++ show(e)
     show (FunctionEquivalence e) = "cannot use equals/non-equals operator on lambda expressions "++ show(e) 


------------------------------------------------------------------------
-- Parser
------------------------------------------------------------------------


appKeywords = ["and", "then", "in", "else", "or"]

keywords = ["lambda", "if", "let" , "rec",
            "not", "fst", "snd"]
            
bools = ["true", "false"]

reservedNames' = keywords ++ bools ++ appKeywords

reservedOperators = ["+", "-", "*", "/", "and", "or", "==",
                     " ", "<", "=", "not", "fst", "snd", 
                     "$", ":", ","]

languageDef =
  emptyDef {
             Token.identStart      = letter
           , Token.identLetter     = alphaNum'
           , Token.reservedNames   = reservedNames'
           , Token.reservedOpNames = reservedOperators
           }

lexer = Token.makeTokenParser languageDef

-- all of these skip trailing whitespace
identifier = Token.identifier lexer -- parses an identifier
parens     = Token.parens     lexer -- parses surrounding parenthesis
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
integer    = Token.integer    lexer -- parses an integer
whiteSpace = Token.whiteSpace lexer -- parses whitespace
colon      = Token.colon      lexer -- parses a colon
dot        = Token.dot        lexer -- parses a dot
natural    = Token.natural    lexer -- parses a positive whole number

opExpr :: Parser Expr
opExpr = buildExpressionParser operators expression

operators = 
    [  [Prefix (reservedOp "-"   >> return (ExprUnOp Neg      ))         ,
        Prefix (reservedOp "not" >> return (ExprUnOp Not      ))         ,
        Prefix (reservedOp "fst" >> return (ExprUnOp Fst      ))         ,
        Prefix (reservedOp "snd" >> return (ExprUnOp Snd      ))         ,
        Infix  (appOp            >> return (App               )) AssocLeft]
     , [Infix  (leadSpaces "*"   >> return (ExprBinOp Times   )) AssocLeft,
        Infix  (leadSpaces "/"   >> return (ExprBinOp Div     )) AssocLeft]
     , [Infix  (leadSpaces "+"   >> return (ExprBinOp Plus    )) AssocLeft,
        Infix  (leadSpaces "-"   >> return (ExprBinOp Minus   )) AssocLeft]
     , [Infix  (leadSpaces "=="  >> return (ExprBinOp Equal   )) AssocLeft,
        Infix  (leadSpaces "!="  >> return (ExprBinOp NotEqual)) AssocLeft,
        Infix  (leadSpaces "<"   >> return (ExprBinOp Lt      )) AssocLeft,
        Infix  (leadSpaces ">"   >> return (ExprBinOp Gt      )) AssocLeft,
        Infix  (leadSpaces "<="  >> return (ExprBinOp Lte     )) AssocLeft,
        Infix  (leadSpaces ">="  >> return (ExprBinOp Gte     )) AssocLeft]
     , [Infix  (leadSpaces "and" >> return (ExprBinOp And     )) AssocLeft]
     , [Infix  (leadSpaces "or"  >> return (ExprBinOp Or      )) AssocLeft]
     ]

-- Parses spaces and then a reserved operator
-- Occurs when there is an application, since we do not parse the spaces
leadSpaces s = try (spaces >> (reservedOp s))

appOp = try (do 
    reservedOp " "
    spaces
    notFollowedBy (eof)
    notFollowedByStrs appKeywords
    notFollowedByStrs reservedOperators
    lookAhead (try (many1 anyChar))
    )

notFollowedByStrs [] = do
    notFollowedBy (char '$')
notFollowedByStrs (x:xs) = do 
    notFollowedBy (reserved x)
    notFollowedByStrs xs

isAlphaNum' a = isAlphaNum a || a == '\''
alphaNum' = try alphaNum <|> satisfy isAlphaNum'

-- these implemented to not skip trailing whitespace
idTrail :: Parser VarName
idTrail = do
    spaces
    fc <- firstChar
    rest <- many nonFirstChar
    if fc:rest `elem` reservedNames' then 
        error $ "Keyword used as variable name: " ++ (fc:rest)
    else do
    return (fc:rest)
  where
    firstChar = satisfy (\a -> isLetter a)
    nonFirstChar = satisfy isAlphaNum' 

{-
parensTrail :: Parser a
parensTrail = between (char '(') (char ')') -- parses surrounding parenthesis
-}

parensTrail = between (char '(') (char ')')

expression = 
        ifStmt 
    <|> letStmt
    <|> lambdaExpr
    <|> try bTerm
    <|> try aTerm
    <|> try tupleTerm
    <|> liftM Var idTrail
    <|> try (parensTrail typeDecExpr)
    <|> try (parensTrail opExpr)

lambdaExpr = do
    reserved "lambda"
    args <- sepEndBy1 typeDecExpr whiteSpace
    dot
    body <- opExpr
    return $ chainArgs args body

chainArgs [] body = body
chainArgs ((TypeDec (Var name) t):xs) body = 
    Lambda name t $ chainArgs xs body
chainArgs _ _ = error "unexpected input"

letStmt = do
    reserved "let"
    var <- identifier
    reservedOp "="
    e1 <- opExpr
    spaces
    reserved "in"
    e2 <- opExpr
    spaces
    return $ Let var e1 e2

bTerm = 
        (string "true"  >> return (Bool True ))
    <|> (string "false" >> return (Bool False))

aTerm = do
    n <- nat
    return $ Num n

tupleTerm = parensTrail tTerm

tTerm = do
    e1 <- opExpr
    spaces
    reservedOp ","
    e2 <- opExpr
    return $ Tuple e1 e2

ifStmt :: Parser Expr
ifStmt = do
    reserved "if"
    cond  <- opExpr
    spaces
    reserved "then"
    stmt1 <- opExpr
    spaces
    reserved "else"
    stmt2 <- opExpr
    return $ If cond stmt1 stmt2

typeDecExpr = 
        parensTrail typeDec
    <|> typeDec

typeDec = do
    s <- opExpr
    spaces
    colon
    t <- typeStatement
    return $ TypeDec s t

-- Type parsers
typeExpr =
        parensTrail typeStatement    
    <|> (reserved "int" >> return (Int))
    <|> (reserved "bool" >> return (Boolean))

typeStatement :: Parser Type
typeStatement = buildExpressionParser typeOp typeExpr

typeOp =    [[Infix (reservedOp "->" >> return (Arrow))      AssocRight],
             [Infix (reservedOp ","  >> return (TupleType))  AssocLeft]]

parseType :: Parser Type -> String -> Type
parseType p str =
  case parse p "" str of
    Left e  -> error $ show e
    Right r -> r

parseExpr :: Parser Expr -> String -> Expr
parseExpr p str =
  case parse p "" str of
    Left e  -> error $ show e
    Right r -> r

parseString :: String -> Expr
parseString str =
  case parse expression "" str of
    Left e  -> error $ show e
    Right r -> r

parser = spaces >> opExpr

------------------------------------------------------------------------
-- Type Checker
------------------------------------------------------------------------


intOps = [Plus, Times, Div, Minus]
compOps = [Lt, Gt, Lte, Gte]

typeOf :: Map VarName Type -> Expr -> Either Error Type
typeOf g e@(Var v) = 
    case (Map.lookup v g) of 
        Nothing -> Left $ UnboundVariable e 
        Just x -> Right x
typeOf g (Num i) = Right Int
typeOf g (Bool b) = Right Boolean
typeOf g (Tuple e1 e2) = 
    case (typeOf g e1) of
        Left e -> Left e
        Right t1 -> case (typeOf g e2) of
            Left e -> Left e
            Right t2 -> Right $ TupleType t1 t2
typeOf g ex@(ExprUnOp u e) 
 | (u == Neg) && ((typeOf g e)==(Right Int)) = Right Int
 | (u == Not) && ((typeOf g e)==(Right Boolean)) = Right Boolean
 | (u == Fst) = case typeOf g e of
                    Right (TupleType t1 t2) -> Right t1
                    _                       -> Left $ TypeMismatch ex
 | (u == Snd) = case typeOf g e of
                    Right (TupleType t1 t2) -> Right t2
                    _                       -> Left $ TypeMismatch ex
typeOf g ex@(ExprBinOp op e1 e2) 
 | op `elem` intOps && ((typeOf g e1) == Right Int) 
                   && ((typeOf g e2) == Right Int) = Right Int
 | op `elem` intOps    = Left  $ TypeMismatch ex
 | op `elem` compOps && ((typeOf g e1) == Right Int) 
                   && ((typeOf g e2) == Right Int) = Right Boolean
 | op `elem` compOps    = Left  $ TypeMismatch ex
 | op `elem` [And, Or] && ((typeOf g e1) == Right Boolean) 
                   && ((typeOf g e2) == Right Boolean) = Right Boolean
 | op `elem` [Equal, NotEqual] && ( (typeOf g e1) == (typeOf g e2) ) = 
                                 if (hasArrow (typeOf g e1) || hasArrow (typeOf g e2) ) 
                                 then Left $ FunctionEquivalence ex
                                 else Right Boolean
 | otherwise    =  Left $ TypeMismatch ex
typeOf g ex@(TypeDec e t) =
    if (typeOf g e) == (Right t) then Right t else Left $ TypeMismatch ex
typeOf g ex@(If e1 e2 e3) -- is this first case on line 320 a catch-all for good outcomes?
 | ( (typeOf g e1) == Right Boolean ) && ((typeOf g e2) == (typeOf g e3) ) = typeOf g e2
 | otherwise  = Left $ TypeMismatch ex
typeOf g ex@(App e1 e2) = 
    let t2 = typeOf g e2
        t1 = typeOf g e1
    in   -- pretty sure this case statement catches all we need 
        case t1 of
            Right (Arrow t11 t22) -> if (Right t11) == t2 then (Right t22) else Left $ TypeMismatch ex
            Left e                -> Left e
typeOf g ex@(Lambda v t1 e) =  -- does this lambda case miss any errors? are they all handled
    case (typeOf (Map.insert v t1 g) e) of -- in the 'let' statement?
        Right t2 -> Right (Arrow t1 t2)
        Left e   -> Left e
typeOf g ex@(Let v e1 e2) = 
    case (typeOf g e1) of
        Right t1 -> typeOf (Map.insert v t1 g) e2
        Left e   -> Left e

hasArrow :: Either Error Type -> Bool
hasArrow typ = 
    case typ of
        (Right (Arrow _ _))              -> True
        (Right (TupleType leftT rightT)) -> (hasArrow (Right leftT)) || (hasArrow (Right rightT))
        _                                -> False


------------------------------------------------------------------------
-- Interpreter
------------------------------------------------------------------------


--interp :: Expr -> Expr 
--interp (Var var)        = Var var
--interp (Lambda var t e)   = Lambda var t e
--interp (App e1 e2)      = 
--    case interp e1 of
--        (Lambda var' e')  -> let !e2' = interp e2 in interp (subst e' var' e2')
--        (App a b)         -> let !e2' = interp e2 in App (App a b) e2'
--        (Var a)           -> let !e2' = interp e2 in App (Var a) e2'

--subst :: Expr -> VarName -> Expr -> Expr
--subst (Var orig) var sub     = if orig == var then sub else (Var orig)
--subst (App e1 e2) var sub    = App (subst e1 var sub) (subst e2 var sub)
--subst (Lambda v e) var sub   = 
--    case v == var of
--        True -> (Lambda v e)
--        False -> (Lambda v (subst e var sub))
--subst e v s                  = error $ "recieved wrong arguments" ++ show(e) ++v ++show(s)


--data Expr = 
--      Var VarName
--    | App Expr Expr
--    | Lambda VarName Type Expr
--    | Let VarName Expr Expr
--    | If Expr Expr Expr
--    | TypeDec Expr Type
--    | Num Integer
--    | Bool Bool
--    | Tuple Expr Expr
--    | ExprUnOp UnOp Expr
--    | ExprBinOp BinOp Expr Expr
--    deriving (Eq, Ord, Show)




--------------------------------------------------------------------------
---- Main
--------------------------------------------------------------------------

--testInterp :: String -> Expr
--testInterp s = case parse lambdaExpr "" s of 
--    (Right v) -> (interp v); 
--    (Left e) -> error (show e)

--main :: IO ()
--main = do
--    args <- getArgs
--    let cflag = any (\a -> a=="-c" || a=="-cn") args 
--    let nflag = any (\a -> a=="-n" || a=="-cn") args
--    let fileArg = Prelude.filter (\a -> a/="-c" && a/="-n" && a/="-cn") args
    
--    if length fileArg > 1 then error "Usage error: too many args"
--    else do
--    input <- if head fileArg == "-" || length fileArg == 0 then getContents
--             else readFile (head fileArg)
--    let ast = case parse lambdaExpr "" input of 
--                   (Right v) -> v 
--                   (Left e) -> error (show e)
--    let output = interp ast
--{-
--    if cflag && (unbounded ast) then 
--        error "Unbound variables"
--    else if nflag then do
--        putStrLn (show (fullChurch output))
--    else do-}
--    putStrLn (show (output))




























