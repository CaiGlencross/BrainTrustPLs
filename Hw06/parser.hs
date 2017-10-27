import Text.Parsec.String
import Text.Parsec.Char
import Data.Char
import Text.Parsec.Combinator
import Text.Parsec

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

lambda :: Parser Expr

-- expr :: Parser Expr
-- expr = var <|>

var :: Parser Expr
var = do
    fc <- firstChar
    rest <- many nonFirstChar
    return $ Var (fc:rest)
  where
    firstChar = satisfy (\a -> isLetter a)
    nonFirstChar = satisfy (\a -> isAlphaNum a || a == '\'')

main :: IO ()
main = undefined
