module Language.HsLisp.Type
    (LispVal
        (Atom
        ,List
        ,DottedList
        ,Number
        ,String
        ,Bool
        ,PrimitiveFunc
        ,Func
        ,IOFunc
        ,Port)
    ,LispError
        (NumArgs
        ,TypeMismatch
        ,Parser
        ,BadSpecialForm
        ,NotFunction
        ,UnboundVar
        ,Default)
    ,Env
    ,ThrowsError
    ,IOThrowsError
    ,liftThrows
    ,runIOThrows
    ) where

import Data.IORef
import Control.Monad.Error
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { _params :: [String],
                      _vararg :: (Maybe String),
                      _body :: [LispVal],
                      _closure :: Env }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom name) = name
    show (Number contents) = show contents
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList headValue tailValue) = "(" ++ unwordsList headValue ++ " . " ++ show tailValue ++ ")"
    show (PrimitiveFunc _) = "<primitive>"
    show (Func {_params = args, _vararg = varargs}) =
        "(lambda (" ++ unwords (map show args) ++ (case varargs of
                                                        Nothing -> ""
                                                        Just arg -> " . " ++ arg) ++ ") ...)"
    show (Port _) = "<IO port>"
    show (IOFunc _) = "<IO primitive>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
    show (UnboundVar message varname) = message ++ ": " ++ varname
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func) = message ++ ": " ++ show func
    show (NumArgs expected found) = "Expected " ++
                                         show expected ++
                                         " args; found values " ++
                                         unwordsList found
    show (TypeMismatch expected found) = "Invalid type: expected " ++
                                              expected ++
                                              ", found " ++
                                              show found
    show (Parser parseErr) = "Parse error at " ++ show parseErr

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default


type Env = IORef [(String, IORef LispVal)]

type ThrowsError = Either LispError

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue
