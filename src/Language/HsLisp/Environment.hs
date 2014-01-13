module Language.HsLisp.Environment
    (primitiveBindings
    ,bindVars
    ,eval
    ) where

import Data.IORef (newIORef)

import Language.HsLisp.Type
import Language.HsLisp.Primitive

nullEnv :: IO Env
nullEnv = newIORef []

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeF IOFunc) ioPrimitives ++ map (makeF PrimitiveFunc) primitives)
                        where makeF constructor (var, func) = (var, constructor func)
