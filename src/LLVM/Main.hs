module Main where

import LLVMConstants

main :: IO ()
main = putStr $ unlines $ concat [header, printConstant(15), footer]