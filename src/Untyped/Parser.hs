{-# LANGUAGE ImportQualifiedPost #-}

module Untyped.Parser
  ( parseExpr,
    parseProgram,
    parseTopLevel,
  )
where

import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token qualified as P
import Untyped.Syntax

lexer :: P.TokenParser ()
lexer =
  P.makeTokenParser
    emptyDef
      { P.commentLine = "--",
        P.reservedOpNames = [":="],
        P.reservedNames = ["let"]
      }

parens :: Parser a -> Parser a
parens = P.parens lexer

lexeme :: Parser a -> Parser a
lexeme = P.lexeme lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

identifier :: Parser Name
identifier = P.identifier lexer

dot :: Parser String
dot = P.dot lexer

lambda :: Parser String
lambda = P.symbol lexer "λ"

pVar :: Parser Term
pVar = Var <$> identifier

pAbs :: Parser Term
pAbs = do
  _ <- lambda
  vars <- many1 identifier
  _ <- dot
  body <- pApp
  return $ foldr Abs body vars

pApp :: Parser Term
pApp = foldl1 App <$> many1 go
  where
    go = parens pApp <|> pAbs <|> pVar

pLetDecl :: Parser TopLevel
pLetDecl = do
  _ <- reserved "let"
  name <- identifier
  _ <- reservedOp ":="
  LetDecl name <$> pApp

contents :: Parser a -> Parser a
contents p = whiteSpace *> lexeme p <* eof

parseExpr :: String -> Either ParseError Term
parseExpr = parse (contents (try pApp)) ""

parseTopLevel :: String -> Either ParseError TopLevel
parseTopLevel = parse (contents (try pLetDecl)) ""

parseProgram :: String -> Either ParseError Program
parseProgram = parse (contents (try $ many pLetDecl)) ""
