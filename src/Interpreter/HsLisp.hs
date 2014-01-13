module Interpreter.HsLisp
    (runOne) where

import System.IO

import Language.HsLisp.Type
import Language.HsLisp.Environment

runOne :: [String] -> IO ()
runOne args = do env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
                 (runIOThrows $ fmap show $ eval env (List [Atom "load", String (args !! 0)])) >>= hPutStrLn stderr
