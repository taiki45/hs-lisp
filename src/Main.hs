module Main
    (main)
where

import System.Environment (getArgs)
import Interpreter.HsLisp
import Interpreter.Shell

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args
