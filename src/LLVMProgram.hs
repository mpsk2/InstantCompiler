module LLVMProgram where

import System.IO
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Error.Class
import qualified Data.Map as Map

import LLVMConstants
import Commons
import MErrors

{-

newtype Ident = Ident String deriving (Eq,Ord,Show,Read)
data Program =
   Prog [Stmt]
  deriving (Eq,Ord,Show,Read)

data Stmt =
   SAss Ident Exp
 | SExp Exp
  deriving (Eq,Ord,Show,Read)

data Exp =
   ExpAdd Exp Exp
 | ExpSub Exp Exp
 | ExpMul Exp Exp
 | ExpDiv Exp Exp
 | ExpLit Integer
 | ExpVar Ident
  deriving (Eq,Ord,Show,Read)



-}

evalProgram :: Program -> MyRunner [String]
evalProgram (Prog stmts) = evalProgram' stmts []
  where
    evalProgram' :: [Stmt] -> [[String]] -> MyRunner [String]
    evalProgram' [] acc = return $ concat $ reverse acc
    evalProgram' (h:t) acc = do
      last_instructions <- evalStmt h
      evalProgram' t (last_instructions : acc)
      
evalStmt :: Stmt -> MyRunner [String]
evalStmt _ = throwError $ NotImplementedError "Stmts"