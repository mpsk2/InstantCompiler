module Main where

import Cli
import System.Environment

main :: IO()
main = getArgs >>= parse