module Commons where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Error.Class

import Instant.Abs
import MErrors


data MyState = MyState {
	env :: Map.Map Ident Integer, 
	nextLocal :: Integer, 
	biggestStack :: Integer
	} deriving (Show)

data SimpleState = SimpleState { 
  nextRegister :: Integer, 
  existingNames :: Set.Set Ident 
  } deriving (Show)

initMyState :: MyState
initMyState = MyState Map.empty 1 0

initSimpleState :: SimpleState
initSimpleState = SimpleState 1 Set.empty

type MyRunner a = StateT MyState (ErrorT CustomError IO) a
type MySimpleRunner a = StateT SimpleState (ErrorT CustomError IO) a

runMyRunner :: MyRunner a -> MyState -> IO (Either CustomError (a, MyState))
runMyRunner m st = runErrorT (runStateT m st)

runMySimpleRunner :: MySimpleRunner a -> SimpleState -> IO (Either CustomError (a, SimpleState))
runMySimpleRunner m st = runErrorT (runStateT m st)

data Operation = Add | Sub | Mul | Div deriving Eq
data Value = Value Integer | Register [String] Integer deriving (Show, Eq)

instance Show Operation where
  show Add = "add"
  show Sub = "sub"
  show Mul = "mul"
  show Div = "sdiv"