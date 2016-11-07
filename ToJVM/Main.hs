module Main where

import Cli
import System.Environment

type Verbosity = Int

main :: IO()
main = do
    args <- getArgs
    let (actions, nonOptions, errors) =  getOptions args
    opts <- foldl (>>=) (return startOptions) actions
    
    putStrLn $ show opts