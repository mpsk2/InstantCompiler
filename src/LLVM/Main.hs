module Main where

import Cli
import LLVMProgram

import MErrors
import System.Environment
import System.Process

main :: IO ()
main = do
    args <- getArgs
    let (actions, nonOptions, errors) =  getOptions args
    opts <- foldl (>>=) (return startOptions) actions
    
    validateArgs nonOptions
    
    runFile (head nonOptions) "test.ll"
    
    return ()