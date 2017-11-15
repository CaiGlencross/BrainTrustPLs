import System.IO
import Control.Monad
import Data.Char

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

type VarName = String

data BExp = 
      Bool Bool
    | Not BExp
    | And BExp BExp
    | Or BExp BExp
    | Lt AExp AExp
    | Gt AExp AExp
    | Lte AExp AExp 
    | Gte AExp AExp
    | AEqual AExp AExp
    | BEqual BExp BExp
    | NAEqual AExp AExp
    | NBEqual BExp BExp
    deriving (Show)

data AExp = 
      Var VarName
    | Num Integer
    | Neg AExp
    | Plus AExp AExp
    | Times AExp AExp
    | Div AExp AExp
    | Minus AExp AExp 
    deriving (Show)

-- need to add tuples back in!!

data Type =
      Int
    | Boolean
    | Arrow Type Type
    deriving (Show)

data Stmt =
      App Stmt Stmt
    | Lambda VarName Type Stmt
    | If BExp Stmt Stmt
    | TypeDec Stmt Type
    deriving (Show)

alphaNum' = try alphaNum <|> satisfy (\a -> isAlphaNum a || a == '\'')

languageDef =
  emptyDef {
             Token.identStart      = letter
           , Token.identLetter     = alphaNum'
           , Token.reservedNames   = [ "lambda", "if", "let" , "rec", "in", "then", 
                                       "else", "true", "false", "not", "and", "or",
                                       "fst", "snd"]
           , Token.reservedOpNames = ["+", "-", "*", "/", "and", "or", "=="
                                     , "<", "=", "not", "fst", "snd"]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
whiteSpace = Token.whiteSpace lexer -- parses whitespace

statement :: Parser Stmt
statement = ifStmt <|> lambdaStmt

ifStmt :: Parser Stmt
ifStmt = do
    reserved "if"
    cond  <- bExpression
    reserved "then"
    stmt1 <- statement
    reserved "else"
    stmt2 <- statement
    return $ If cond stmt1 stmt2

lambdaStmt = undefined

aExpression :: Parser AExp
aExpression = buildExpressionParser aOperators aTerm
 
bExpression :: Parser BExp
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Prefix (reservedOp "-"   >> return (Neg             ))          ]
             , [Infix  (reservedOp "*"   >> return (Times)) AssocLeft,
                Infix  (reservedOp "/"   >> return (Div  )) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (Plus     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (Minus)) AssocLeft]
              ]
 
bOperators = [ [Prefix (reservedOp "not" >> return (Not             ))          ]
             , [Infix  (reservedOp "and" >> return (And     )) AssocLeft,
                Infix  (reservedOp "or"  >> return (Or      )) AssocLeft]
             ]

aTerm =  parens aExpression
     <|> liftM Var identifier
     <|> liftM Num integer

bTerm =  parens bExpression
     <|> (reserved "true"  >> return (Bool True ))
     <|> (reserved "false" >> return (Bool False))
     <|> aExpComp
     <|> bExpComp

aExpComp =
  do a1 <- aExpression
     op <- aExpRelation
     a2 <- aExpression
     return $ op a1 a2
 
aExpRelation =   (reservedOp ">" >> return Gt)
             <|> (reservedOp "<" >> return Lt)
             <|> (reservedOp "<=" >> return Lte)
             <|> (reservedOp ">=" >> return Gte)
             <|> (reservedOp "!=" >> return NAEqual)
             <|> (reservedOp "==" >> return AEqual)

bExpComp =
  do a1 <- bExpression
     op <- bExpRelation
     a2 <- bExpression
     return $ op a1 a2
 
bExpRelation =   (reservedOp "!=" >> return NBEqual)
             <|> (reservedOp "==" >> return BEqual)

parseString :: String -> AExp
parseString str =
  case parse aExpression "" str of
    Left e  -> error $ show e
    Right r -> r