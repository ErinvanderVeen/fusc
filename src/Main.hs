module Main where

import Fuspel.Lexer
import Fuspel.Parser
import System.Environment
import System.Exit
import Text.Parsec.Error (ParseError)
import Prelude hiding (lex)

main :: IO ()
main = do
  files <- getArgs
  programs <- mapM parseFuspelFile files
  mapM_ print programs
