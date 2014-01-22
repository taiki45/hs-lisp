module Language.HsLisp.Parser
    (readExpr
    ,readExprList
    ) where

import Control.Applicative ((<*), (*>), (<$>), (<*>), pure)
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)
import Language.HsLisp.Type

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escape :: Parser Char
escape = char '\\' <* oneOf "\\\""

parseString :: Parser LispVal
parseString = pure String
              <*> (char '"'
                  *> many (escape <|> noneOf "\"") <*
                  char '"')

parseAtom :: Parser LispVal
parseAtom = do atom <- pure (:)
                       <*> (letter <|> symbol)
                       <*> many (letter <|> digit <|> symbol)
               return $ case atom of
                            "#t" -> Bool True
                            "#f" -> Bool False
                            _    -> Atom atom


parseNumber :: Parser LispVal
parseNumber = (Number . read) <$> many1 digit

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = pure DottedList
                  <*> endBy parseExpr spaces
                  <*> (char '.' *> spaces *> parseExpr)

parseQuoted :: Parser LispVal
parseQuoted = do x <- char '\'' *> parseExpr
                 return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
            <|> parseQuoted
            <|> (char '('
                *> (try parseList <|> parseDottedList) <*
                char ')')

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
                                Left err -> throwError $ Parser err
                                Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow $ endBy parseExpr spaces
