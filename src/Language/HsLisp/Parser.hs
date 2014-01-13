module Language.HsLisp.Parser
    (readExpr
    ,readExprList
    ) where

import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)
import Language.HsLisp.Type

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escape :: Parser Char
escape = char '\\' >> oneOf "\\\""

parseString :: Parser LispVal
parseString = do char '"'
                 str <- many $ escape <|> noneOf "\""
                 char '"'
                 return $ String str

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                            "#t" -> Bool True
                            "#f" -> Bool False
                            _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = fmap (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = fmap List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
        headValue <- endBy parseExpr spaces
        tailValue <- char '.' >> spaces >> parseExpr
        return $ DottedList headValue tailValue

parseQuoted :: Parser LispVal
parseQuoted = do
        char '\''
        x <- parseExpr
        return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom <|>
            parseString <|>
            parseNumber <|>
            parseQuoted <|>
            do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)
