module Commons where

import qualified Data.Map as Map
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
	
initMyState :: MyState
initMyState = MyState Map.empty 1 0

type MyRunner a = StateT MyState (ErrorT CustomError IO) a

runMyRunner :: MyRunner a -> MyState -> IO (Either CustomError (a, MyState))
runMyRunner m st = runErrorT (runStateT m st)