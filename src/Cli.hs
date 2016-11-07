module Cli where

import Control.Monad
import Data.Char
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

tac :: String -> String
tac = unlines . reverse . lines

usage :: IO ()
usage = putStrLn "TODO" >> exitFailure

data Flag
	= Output -- -o/--output
	| Help   -- -h/--help
	deriving (Eq, Ord, Enum, Show, Bounded)

flags = 
	[Option ['o'] ["output"] (NoArg Output) "Output file"
	,Option ['h'] ["help"] (NoArg Help) "Print help message"
	]

parse argv = case getOpt Permute flags argv of
		(args,fs,[]) -> do
			hPutStrLn stderr (usageInfo header flags)
			exitWith ExitSuccess
		(_,_,errs) -> do
			exitWith (ExitFailure 1)
        where header = "Usage: cat [-benstuv] [file ...]"