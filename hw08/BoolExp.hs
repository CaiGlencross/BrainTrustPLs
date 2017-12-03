{-
Syntax and Implementation of Boolean Expressions
================================================
-}

module BoolExp where

import Pi
import qualified Data.Map.Strict as M

data BoolExp
  = BVar Name
  | BVal Bool
  | BoolExp :&&: BoolExp
  | BoolExp :||: BoolExp
  | Not BoolExp
  deriving Show

-- Environments for interpreting boolean expressions
type BEnv = M.Map Name Bool

-- TASK!
-- compileBExp tchan fchan b
-- returns a process p that when juxtaposed with a compatible environment
-- sends a message on tchan if the boolean expression evaluates to true
-- sends a message on fchan if the boolean expression evaluates to false
genName :: Int -> Name
genName n = "chan" ++ (show n)

compileBExp :: Name -> Name -> BoolExp -> Pi
compileBExp tchan fchan bExp = compileBExp' 0 tchan fchan bExp 

compileBExp' :: Int -> Name -> Name -> BoolExp -> Pi
compileBExp' n tchan fchan (BVal v) = if (v)
                                    then (Out tchan (unitE))
                                    else (Out fchan (unitE))

compileBExp' n tchan fchan (Not b)  = compileBExp' n fchan tchan b

compileBExp' n tchan fchan (a :&&: b) = New (genName n) unitT 
                                  ( (compileBExp' (n+1) (genName n) fchan a) :|:
                                    (compileBExp' (n+1) (genName n) fchan b) :|:
                                      (Inp (genName n) unitP 
                                      (Inp (genName n) unitP 
                                      (Out tchan unitE))))

compileBExp' n tchan fchan (a :||: b) = compileBExp' n tchan fchan (Not (Not a :&&: Not b))

--set up the compileBoolEnv to handle this tomorrow
compileBExp' n tchan fchan (BVar name) = undefined




-- TASK!
-- compile a boolean variable environment into a process that
-- communicates with a compiled Boolean expression containing free
-- variables from the environment
compileBExpEnv :: BEnv -> Pi -> Pi
compileBExpEnv benv p = undefined

startBool :: BEnv -> BoolExp -> IO ()
startBool benv bexp =
  start pi
    where
      tchan = "t"
      fchan = "f"
      pi = New tchan unitT $
           New fchan unitT $
           compileBExpEnv benv (compileBExp tchan fchan bexp) :|:
           Inp tchan unitP (printer "true") :|:
           Inp fchan unitP (printer "false")
