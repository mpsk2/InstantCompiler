module JasminProgram where

import System.IO
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Error.Class

import qualified Data.Map as Map

import JasminConstants
import MErrors

import Instant.Abs
import Instant.Par 
import Instant.ErrM

runFile :: String -> String -> String -> IO (Either CustomError ((), MyState))
runFile inputFile outputFile className = readFile inputFile >>= actualProgram outputFile className 

actualProgram :: String -> String -> String -> IO (Either CustomError ((), MyState))
actualProgram path className program = runMyRunner (parseProgram program className path) initMyState
  
data MyState = MyState {env :: Map.Map Ident Integer, nextLocal :: Integer, biggestStack :: Integer} deriving (Show)

initMyState :: MyState
initMyState = MyState Map.empty 1 0

type MyRunner a = StateT MyState (ErrorT CustomError IO) a

runMyRunner :: MyRunner a -> MyState -> IO (Either CustomError (a, MyState))
runMyRunner m st = runErrorT (runStateT m st)

parseProgram :: String -> String -> String -> MyRunner ()
parseProgram s className path = let ts = myLexer s in
  case pProgram ts of
    Bad s -> do
      liftIO $ print s
      throwError (OtherError s)
    Ok three -> do
      liftIO $ print three
      program <- evalProgram three
      state <- get
      liftIO $ print state
      liftIO $ writeFile path $ unlines ((header className) ++ (mainHeader (biggestStack state) (nextLocal state)) ++ program ++ mainFooter)
  
evalProgram :: Program -> MyRunner [String]
evalProgram (Prog stmts) = evalProgram' stmts []
  where
    evalProgram' :: [Stmt] -> [[String]] -> MyRunner [String]
    evalProgram' [] acc = return $ concat $ reverse acc
    evalProgram' (h:t) acc = do
      last_instructions <- evalStmt h
      evalProgram' t (last_instructions : acc)

evalStmt :: Stmt -> MyRunner [String]
evalStmt (SAss ident e) = do
  (instructions, stack_size) <- evalExp e
  sub_state <- get
  put $ MyState (env sub_state) (nextLocal sub_state) (max (biggestStack sub_state) stack_size)
  state <- get
  if Map.member ident (env state)
     then return $ instructions ++ (saveToLocal ((env state) Map.! ident))
     else do
       put $ MyState (Map.insert ident (nextLocal state) (env state)) ((nextLocal state) + 1) (biggestStack state) -- TODO
       state2 <- get
       return $ instructions ++ (saveToLocal ((env state2) Map.! ident))
       
evalStmt (SExp e) = do
  state <- get
  (instructions, stackSize) <- evalExp e
  put $ MyState (env state) (nextLocal state) (max (stackSize + 1) (biggestStack state)) -- TODO(mps)
  return $ printInt instructions
  
evalExp :: Exp -> MyRunner ([String], Integer)
evalExp (ExpLit i) = return $ (intToStack i, 1)
evalExp (ExpAdd lhs rhs) = evalArith lhs rhs Add 
evalExp (ExpSub lhs rhs) = evalArith lhs rhs Sub 
evalExp (ExpMul lhs rhs) = evalArith lhs rhs Mul  
evalExp (ExpDiv lhs rhs) = evalArith lhs rhs Div  
evalExp (ExpVar ident) = do
  state <- get
  return (loadLocal $ ((env state) Map.! ident), 1)

evalArith :: Exp -> Exp -> Operation -> MyRunner ([String], Integer)
evalArith lhs rhs op = do
  (lhs_inst, lhs_stack) <- evalExp lhs
  (rhs_inst, rhs_stack) <- evalExp rhs
  if lhs_stack > rhs_stack
    then return (operation lhs_inst rhs_inst op, rhs_stack + 1) -- TODO - CALCULATE STACK
    else return (operation rhs_inst lhs_inst op, lhs_stack + 1) -- TODO - CALCULATE STACK