module Main where

import Cli
import JasminProgram
import MErrors
import System.Environment
import System.Process
import System.FilePath.Posix

main :: IO ()
main = do
    args <- getArgs
    let (actions, nonOptions, errors) =  getOptions args
    opts <- foldl (>>=) (return startOptions) actions
    
    validateArgs nonOptions
    
    putStrLn $ jasminOutput $ head nonOptions
    
    runFile (head nonOptions) (jasminOutput $ head nonOptions) (baseName $ head nonOptions)
    
    callProcess "java" ["-jar", "libs/jasmin.jar", jasminOutput $ head nonOptions, "-d", takeDirectory $ head nonOptions]
