module Main where

import Cli
import LLVMProgram

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
    
    runFile (head nonOptions) (llvmOutput $ head nonOptions)
    
    callProcess "llvm-as" [llvmOutput $ head nonOptions, "-o", "_sub.bc"]
    callProcess "llvm-link" ["-o", bcOutput (head nonOptions) (outputDirectory opts), "libs/runtime.bc", "_sub.bc"]
    callProcess "rm" ["_sub.bc"]
