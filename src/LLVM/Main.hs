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
    
    runFile (head nonOptions) (llvmOutput (head nonOptions) (outputDirectory opts))
    
    callProcess "llvm-as" [llvmOutput (head nonOptions) (outputDirectory opts), "-o", "_sub.bc"]
    callProcess "llvm-link" ["-o", bcOutput (head nonOptions) (outputDirectory opts), "tools/runtime.bc", "_sub.bc"]
    callProcess "rm" ["_sub.bc"]