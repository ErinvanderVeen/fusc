{-# LANGUAGE ScopedTypeVariables #-}
module Fuspel.Parser
  where

import Control.Monad
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.String

import Fuspel.AST
import Fuspel.Lexer

parseFuspelFile :: FilePath -> IO Fuspel
parseFuspelFile fp = do
  res <- parseFromFile fuspelParser fp
  case res of
    Left e  -> error $ show e
    Right r -> return r

parseFuspelString :: String -> Fuspel
parseFuspelString str =
  case parse fuspelParser "" str of
    Left e  -> error $ show e
    Right r -> r

fuspelParser :: Parser Fuspel
fuspelParser = do
  whiteSpace
  imports <- many fimport
  rules <- many rewrite
  return $ Fuspel imports rules

-- Fuspel Import
fimport :: Parser Import
fimport = do
  reserved "import"
  im <- identifier
  semi
  return $ Import im

rewrite :: Parser Rewrite
rewrite = do
  fname <- name
  args <- many simpleExpression
  reservedOp "="
  expr <- expression
  semi
  return $ Rewrite fname args expr

name :: Parser Name
name = do
  name <- identifier
  return name

simpleExpression :: Parser SimpleExpression
simpleExpression =
      liftM SEInt (fromInteger <$> integer)
  <|> liftM SEName identifier
  <|> liftM (uncurry SEList) (list simpleExpression)
  <|> liftM (uncurry SETuple) (tuple simpleExpression)
  <|> wildcard
  where
    wildcard :: Parser SimpleExpression
    wildcard = symbol "_" >> return SEWildCard

list :: forall a. Parser a -> Parser ([a], Maybe a)
list elemParser = (try colonList) <|> commaList
  where
    colonList :: Parser ([a], Maybe a)
    colonList = brackets $ elemParser >>= \hd ->
      colon >> (
        (do
          (tl, end) <- (list elemParser)
          return ((hd:tl), end)
        )
        <|>
        (do
          end <- elemParser
          return ([hd], Just end)
        )
      )

    commaList :: Parser ([a], Maybe a)
    commaList = brackets $ do
      elems <- commaSep elemParser
      tail <- tail
      return (elems, tail)

    tail :: Parser (Maybe a)
    tail =
        (try (colon >> symbol "[]" >> return Nothing))
      <|>
        (colon >> Just <$> elemParser)
      <|>
        (return Nothing)

tuple :: Parser a -> Parser (a, a)
tuple elemParser = do
  symbol "("
  x <- elemParser
  symbol ","
  y <- elemParser
  symbol ")"
  return $ (x, y)

expression :: Parser Expression
expression =
      (try application)
  <|> expression'
  where
    application :: Parser Expression
    application = buildExpressionParser [[Infix (whiteSpace >> return (EApp)) AssocLeft]] expression'

    expression' :: Parser Expression
    expression' =
          liftM EInt (fromInteger <$> integer)
      <|> liftM (uncurry EList) (list expression)
      <|> try (liftM (uncurry ETuple) (tuple expression))
      <|> (parens expression)
      <|> liftM ECode (reserved "code" >> identifier)
      <|> liftM EName identifier
