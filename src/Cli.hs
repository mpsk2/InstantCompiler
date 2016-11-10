module Cli where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.Directory
import System.FilePath.Posix
import Control.Monad
import System.IO
import Data.List
import Data.Char

data Options = Options { outputDirectory :: String 
		       } deriving (Show)

startOptions :: Options
startOptions = Options { outputDirectory = "."
		       }

usageInfoHeader :: String -> String
usageInfoHeader program_name = "Usage: " ++ program_name ++ " file_name [OPTIONS]"
		       
		       
options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "h" ["help"]
    (NoArg
      (\_ -> do
	prg <- getProgName
	hPutStrLn stderr (usageInfo (usageInfoHeader prg) options)
	exitWith ExitSuccess))
      "Show help"
  , Option "o" ["output"]
    (ReqArg
      (\arg opt -> return opt { outputDirectory = arg })
      "DIRECTORY")
    "Output directory"
  ]
  
getOptions args = getOpt RequireOrder options args

argsFail :: IO ()
argsFail = do
  prg <- getProgName
  hPutStrLn stderr (usageInfo (usageInfoHeader prg) options)
  exitWith ExitSuccess

validateArgs :: [String] -> IO ()
validateArgs [] = argsFail
validateArgs [path] = do
  exists' <- doesFileExist path
  if exists'
     then return ()
     else argsFail
validateArgs (_:_:_) = argsFail

jasminOutput :: String -> String
jasminOutput fileName = (dropExtension fileName) ++ ".j"

llvmOutput :: String -> String
llvmOutput fileName = (dropExtension fileName) ++ ".ll"

bcOutput :: String -> String -> String
bcOutput fileName outputDirectory = (dropExtension fileName) ++ ".bc"

classOutput :: String -> String
classOutput fileName = (dropExtension fileName) ++ ".class"

baseName :: String -> String
baseName fileName = takeBaseName fileName 