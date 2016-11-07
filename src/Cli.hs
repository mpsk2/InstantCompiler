module Cli where

import System.Console.GetOpt
import System.Environment
import System.Exit
import Control.Monad
import System.IO
import Data.List
import Data.Char

data Options = Options { outputFile :: String 
		       } deriving (Show)

startOptions :: Options
startOptions = Options { outputFile = "a.out"
		       }

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "h" ["help"]
      (NoArg
	(\_ -> do
	  prg <- getProgName
	  hPutStrLn stderr (usageInfo prg options)
	  exitWith ExitSuccess))
      "Show help"
  ]
  
getOptions args = getOpt RequireOrder options args