module Fuspel.Lexer
( identifier
, symbol
, reserved
, reservedOp
, parens
, brackets
, integer
, whiteSpace
, lexeme
, colon
, semi
, commaSep
) where

import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
import Fuspel.Language

lexer = T.makeTokenParser fuspelDef

identifier = T.identifier lexer
symbol     = T.symbol lexer
reserved   = T.reserved lexer
reservedOp = T.reservedOp lexer
parens     = T.parens lexer
brackets   = T.brackets lexer
integer    = T.integer lexer
whiteSpace = T.whiteSpace lexer
lexeme     = T.lexeme lexer
colon      = T.colon lexer
semi       = T.semi lexer
commaSep   = T.commaSep lexer
