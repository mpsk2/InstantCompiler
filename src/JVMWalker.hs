module JVMWalker where

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Error.Class
import System.Exit

import Instant.Abs
import Instant.Lex 
import Instant.Par 
import Instant.Skel 
import Instant.Print 
import Instant.ErrM
import MErrors

{- Helping Staff TODO(mps): remove
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
  
data MyState = MyState deriving (Show)

initMyState :: MyState
initMyState = MyState

type MR a = StateT MyState (ErrorT CustomError IO) a

runMR :: MR a -> MyState -> IO (Either CustomError (a, MyState))
runMR m st = runErrorT (runStateT m st)

-- TO REMOVE
demo :: MR ()
demo = do
  state <- get
  liftIO $ print state

evalProgram :: Program -> MR ()
evalProgram program = throwError $ NotImplementedError "Program"

evalStmt :: Stmt -> MR ()
evalStmt stmt = throwError $ NotImplementedError "Statement"

evalExp :: Exp -> MR ()
evalExp exp = throwError $ NotImplementedError "Expression"

goProgram' :: Program -> MR ()
goProgram' program = do
    evalProgram program
    state' <- get
    liftIO $ print state'
    
goProgram :: Program -> MR ()
goProgram program = (goProgram' program) `catchError` handler

handler :: CustomError -> MR()
handler err = liftIO $ print (show err)

runProgram :: String -> IO (Either CustomError ((), MyState))
runProgram string = runMR (parseProgram string) initMyState

parseProgram :: String -> MR ()
parseProgram s = let ts = myLexer s in
  case pProgram ts of
    Bad s -> do
      liftIO $ print (show s)
      throwError (OtherError s)
    Ok three -> do
      liftIO $ print three
      evalProgram three
      state <- get
      liftIO $ print state

type ParseFun a = [Token] -> Err a
type Verbosity = Int

myLLexer = myLexer

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ParseFun Program -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          showTree v tree
                          putStrLn "\nBefore Eval!"
                          runMR (goProgram tree) initMyState
                          putStrLn "\nAfter Eval!"
                          exitSuccess

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree
      

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()