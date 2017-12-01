{-# LANGUAGE FlexibleInstances #-}

-- Implementation of the Syntax and Operational Semantics of the Pi Calculus

module Pi where

-- For documentation, see the following pages:
-- http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Concurrent.html
-- http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Concurrent-Chan.html

import Concurrent

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.List (concatMap)

-- Syntax of the Pi Calculus

type Name = String

instance Show (Chan Value) where
  show chan = "<channel>"

-- When reading through these data types, it is worth noting that *all* values
-- in this pi calculus are like locations in the STLC with references: they only
-- show up during evaluation, but *not* in programs a user might write.
--
-- In other words, the "abstract channel" object defined in your handout (as
-- "c" in the syntax) will actually be a Haskell channel (VChan below).  But
-- your translation will generate Pi terms, which only include expressions
-- (Exp), not values.

data Value
  = VChan (Chan Value)  -- channel value
  | VTup [Value]        -- tuple of values
  deriving Show

data Exp
  = EVar Name           -- variable expression
  | ETup [Exp]          -- tuple of expressions
  deriving Show

data Pattern
  = PVar Name           -- variable pattern
  | PTup [Pattern]      -- tuple pattern
  | Wild                -- wildcard pattern
  deriving Show

data Typ
  = TChan Typ           -- channel type
  | TTup [Typ]          -- tuple type
  deriving Eq


--instance Eq Pattern where
--  (PVar nm1) == (PVar nm2)         = nm1 == nm2
--  (PTup tupLst1) == (PTup tupLst2) = tupLst1 == tupLst2
--  _ == _                           = False


instance Show Typ where
  show (TChan t) = "Chan " ++ (show t)
  show (TTup []) = "()"
  show (TTup (h:ts)) = "(" ++ (show h) ++
    (concatMap (\x -> ", " ++ (show x)) ts) ++ ")"

instance Show (Env -> IO ()) where
  show f = "<function>"

data Pi
  = Nil
  | Pi :|: Pi
  | New Name Typ Pi
  | Out Name Exp
  | Inp Name Pattern Pi
  | RepInp Name Pattern Pi   -- repeated input
  | Embed (Env -> IO ()) Pi

instance Show Pi where
  show Nil = "0"
  show (p1 :|: p2) =
    "(" ++ (show p1) ++ ") | (" ++ (show p2) ++ ")"
  show (New x t p) =
    "new " ++ x ++ " : " ++ (show t) ++ ". " ++ (show p)
  show (Out x e) =
    "send " ++ x ++ "(" ++ (show e) ++ ")"
  show (Inp x pat p) =
    "rec " ++ x ++ "(" ++ (show pat) ++ "). " ++ (show p)
  show (RepInp x pat p) =
    "rec! " ++ x ++ "(" ++ (show pat) ++ "). " ++ (show p)
  show (Embed _ p) = "<function> " ++ (show p)

-- Useful Abbreviations

unitT :: Typ
unitT = TTup []

unitE :: Exp
unitE = ETup []

unitP :: Pattern
unitP = PTup []

printer :: String -> Pi
printer s = Embed (\_ -> putStrLn s) Nil

-- Static type checking

-- TASK!
-- Implement your pi calculus type checker here!

type Gamma = Map Name Typ

typeExp :: Gamma -> Exp -> Either String Typ
typeExp = undefined

typePat :: Gamma -> Pattern -> Typ -> Either String Gamma
typePat = undefined

checkPi :: Gamma -> Pi -> Either String ()
checkPi = undefined

check :: Pi -> Either String ()
check p = checkPi Map.empty p

-- Signals a dynamic error

type_error :: String -> a
type_error s = error $ "Run-time Type Error: " ++ s

-- Environments for interpreters

-- TASK!
-- Implement your interpreter here!

type Env = Map Name Value




isDisjoint :: [Name] -> Pattern -> [Name]
isDisjoint s (PVar name)   = if elem name s then s 
                             else type_error "non-disjoint substitution domains"
isDisjoint s (PTup [])     = s
isDisjoint s (PTup ((PVar v):ps) ) = 
        if elem v s then type_error "non-disjoint substitution domains"
        else isDisjoint ([v]++ s) (PTup ps) 
isDisjoint s (PTup ((PTup p):ps) )  = isDisjoint (isDisjoint s (PTup p) ) (PTup ps)





-- evalPat env p v
-- match a value v against a pattern p and extend environment env
evalPat :: Env -> Pattern -> Value -> Env
evalPat env Wild           _               = env
evalPat env (PVar vname)   val             = Map.insert vname val env
evalPat env pt@(PTup (p:ps) ) (VTup (v:vs) )  = 
      case isDisjoint [] pt of
            _ -> evalPat (evalPat env p v) (PTup ps) (VTup vs)
evalPat env (PTup [] )      (VTup [] )     = env
evalPat _ _       _                        = type_error $ "mismatched value; needed list of channels"


-- evalExp env e
-- evaluates e to a value in environment env
evalExp :: Env -> Exp -> Value
evalExp env (EVar x) = env ! x
evalExp env (ETup es) = VTup (evalExps env es)
  where
    evalExps env [] = []
    evalExps env (e:es) = evalExp env e : evalExps env es

run :: Env -> Pi -> IO ()
run env Nil = IO ()
run env (p1 :|: p2) = Concurrent.parallel (run env p1) (run env p2)
run env (New name typ pi) = 
    do
    chan <- newChan
    let env' = Map.insert name (VChan chan) env
    return $ run env' pi
run env (Out name exp) =
    case lookup name env of
        (Right (VChan chan)) -> writeChan chan exp 
        (Right _) -> type_error "sending to nonchannel"
        (Left err)   -> error "output to nonexistent channel"
run env (Inp name pat pi) =
    do
    let chan = case Map.lookup name env of
               (Just chan) -> chan
               _   -> error "input from nonexistent channel"
    val <- readChan chan 
    let env' = evalPat env pat val
    run env' pi
run env (RepInp name pat pi) =
    do
    let chan = case Map.lookup name env of
               (Just (VChan chan)) -> chan
               (Just _) -> type_error "sending to nonchannel"
               _   -> error "input from nonexistent channel"
    val <- readChan chan 
    let env' = evalPat env pat val
    run env' ((RepInp name pat pi) :|: pi)

start :: Pi -> IO ()
start p = run Map.empty p