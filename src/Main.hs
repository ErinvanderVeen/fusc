module Main where

import Prelude hiding(lex)
import System.Environment
import System.Exit

import Text.Parsec.Error (ParseError)

import Fuspel.Lexer
import Fuspel.Parser

main :: IO ()
main = do
  files <- getArgs
  programs <- mapM parseFuspelFile files
  mapM_ print programs
