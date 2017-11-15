import System.IO
import Control.Monad
import Data.Char

import Text.Parsec.Number
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

type VarName = String

data Expr = 
      Var VarName
    | App Expr Expr
    | Lambda VarName Type Expr
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

keywords = ["lambda", "if", "let" , "rec", "in", "then", 
            "else",  "not", "and", "or",
            "fst", "snd"]
            
bools = ["true", "false"]

reservedNames' = keywords ++ bools

languageDef =
  emptyDef {
             Token.identStart      = letter
           , Token.identLetter     = alphaNum'
           , Token.reservedNames   = reservedNames'
           , Token.reservedOpNames = ["+", "-", "*", "/", "and", "or", "==", 
                                      " ", "<", "=", "not", "fst", "snd", 
                                      "$", ":", ","]
           }

lexer = Token.makeTokenParser languageDef

isAlphaNum' a = isAlphaNum a || a == '\''
alphaNum' = try alphaNum <|> satisfy isAlphaNum'

-- these implemented to not skip trailing whitespace
idTrail :: Parser VarName
idTrail = do
    spaces
    fc <- firstChar
    rest <- many nonFirstChar
    if fc:rest `elem` keywords then 
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

-- Parses spaces and then a reserved operator
-- Occurs when there is an application, since we do not parse the spaces
leadSpaces s = try (spaces >> (reservedOp s))

-- this is currently not used
notFollowedByStrs [] = do
    notFollowedBy (char '$')
notFollowedByStrs (x:xs) = do 
    notFollowedBy (reserved x)
    notFollowedByStrs xs

appOp = try (do 
    reservedOp " "
    spaces
    notFollowedBy (eof)
    notFollowedBy (reservedOp ":") 
    notFollowedBy (reservedOp ",")
    notFollowedBy $ reserved "then"
    notFollowedBy $ reserved "in"
    notFollowedBy $ reserved "else"
    notFollowedByStrs ["and"]
    lookAhead (try (many1 anyChar))
    )

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

-- temp solution, we need types!
letStmt = do
    reserved "let"
    var <- identifier
    reservedOp "="
    e1 <- opExpr
    spaces
    reserved "in"
    e2 <- opExpr
    spaces
    return $ App (Lambda var Boolean e2) e1

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
