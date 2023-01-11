module Main (main) where

import REPL
import Untyped.Env

main :: IO ()
main = runRepl empty
