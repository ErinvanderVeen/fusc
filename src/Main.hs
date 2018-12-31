-- vim: et ts=2 sw=2 ai:
module Main where

import Prelude hiding(lex)
import System.Environment
import System.Exit

import Text.Parsec.Error (ParseError)

import Fuspel.Lexer

main :: IO ()
main = do
  putStrLn "Hello, World!"
