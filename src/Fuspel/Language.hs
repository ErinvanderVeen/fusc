module Fuspel.Language
  ( fuspelDef,
  )
where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token

fuspelDef :: LanguageDef st
fuspelDef =
  emptyDef
    { commentStart = "",
      commentEnd = "",
      commentLine = "//",
      nestedComments = False,
      identStart = lower,
      identLetter = alphaNum <|> char '_',
      opStart = parserZero, -- The language doesn't have infix operators
      opLetter = parserZero,
      reservedNames = ["code", "import"],
      reservedOpNames = ["="],
      caseSensitive = True
    }
