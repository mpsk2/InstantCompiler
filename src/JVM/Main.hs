module Main where

import Cli
import JasminProgram
import MErrors
import System.Environment
import System.Process

type Verbosity = Int

main :: IO ()
main = do
    args <- getArgs
    let (actions, nonOptions, errors) =  getOptions args
    opts <- foldl (>>=) (return startOptions) actions
    
    validateArgs nonOptions
    
    runFile (head nonOptions) (jasminOutput (head nonOptions) (outputDirectory opts)) (baseName $ head nonOptions)
    
    callProcess "java" ["-jar", "tools/jasmin.jar", (jasminOutput (head nonOptions) (outputDirectory opts)), "-d", outputDirectory opts]