module LLVMProgram where

import System.IO
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Error.Class
import qualified Data.Set as Set

import LLVMConstants
import Commons
import MErrors

import Instant.Abs
import Instant.Par 
import Instant.ErrM

runFile :: String -> String -> IO (Either CustomError ((), SimpleState))
runFile inputFile outputFile = readFile inputFile >>= actualProgram outputFile 

actualProgram :: String -> String -> IO (Either CustomError ((), SimpleState))
actualProgram path program = runMySimpleRunner (parseProgram program path) initSimpleState

parseProgram :: String -> String -> MySimpleRunner ()
parseProgram s path = let ts = myLexer s in
  case pProgram ts of
    Bad s -> do
      liftIO $ print s
      throwError (OtherError s)
    Ok three -> do
      program <- evalProgram three
      state <- get
      liftIO $ writeFile path $ unlines (header ++ program ++ footer)

evalProgram :: Program -> MySimpleRunner [String]
evalProgram (Prog stmts) = evalProgram' stmts []
  where
    evalProgram' :: [Stmt] -> [[String]] -> MySimpleRunner [String]
    evalProgram' [] acc = return $ concat $ reverse acc
    evalProgram' (h:t) acc = do
      last_instructions <- evalStmt h
      evalProgram' t (last_instructions : acc)
      
evalStmt :: Stmt -> MySimpleRunner [String]
evalStmt (SExp (ExpLit i)) = return $ printConstant i
evalStmt (SExp e) = do
  value <- evalExp e
  case value of
       Register instructions ticket -> return $ instructions ++ (printRegister ticket)
       _ -> throwError $ NotImplementedError "Stmts exp value"
evalStmt s@(SAss i@(Ident name) e) = do
  state <- get
  if Set.member i (existingNames state)
     then do
       value <- evalExp e
       return $ storeValue value name
     else do
       put $ SimpleState (nextRegister state) (Set.insert i (existingNames state))
       withName <- evalStmt s
       return $ (allocateVariable name) ++ withName
       

getTicket :: MySimpleRunner Integer
getTicket = do
  state <- get
  put $ SimpleState ((nextRegister state) + 1) (existingNames state)
  return $ nextRegister state

evalExp :: Exp -> MySimpleRunner Value
evalExp (ExpLit i) = return $ Value i
evalExp (ExpVar (Ident name)) = do
  ticket <- getTicket
  return $ Register (loadVariable name ticket) ticket
evalExp (ExpAdd lhs rhs) = evalArithm lhs rhs Add
evalExp (ExpSub lhs rhs) = evalArithm lhs rhs Sub
evalExp (ExpMul lhs rhs) = evalArithm lhs rhs Mul
evalExp (ExpDiv lhs rhs) = evalArithm lhs rhs Div

evalArithm :: Exp -> Exp -> Operation -> MySimpleRunner Value
evalArithm lhs rhs op = do
  lhs_value <- evalExp lhs
  rhs_value <- evalExp rhs
  ticket <- getTicket
  return $ Register (arithmetic ticket op lhs_value rhs_value) ticket 