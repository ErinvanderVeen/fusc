module Main
  where

import Text.Parsec
import System.Directory

import Test.Tasty
import Test.Tasty.HUnit

import Fuspel.AST
import Fuspel.Lexer
import Fuspel.Parser

main :: IO ()
main = do
  testfiles <- getTestFiles
  defaultMain (fuscTests testfiles)

fuscTests :: [(FilePath, String)] -> TestTree
fuscTests files = testGroup "fusc Tests" [parserTests files]

parserTests :: [(FilePath, String)] -> TestTree
parserTests files = testGroup "Parser Tests" $
    [ simpleExpressionParserTests
    , expressionParserTests
    , parseFileTests files
    ]

getTestFiles :: IO [(FilePath, String)]
getTestFiles = do
  filenames <- listDirectory "test/testfiles/"
  let filepaths = map ((++) "test/testfiles/") filenames
  contents <- mapM readFile filepaths
  return $ zip filenames contents

-- Tests if Show . Parse is the same as the input file
parseFileTests :: [(FilePath, String)] -> TestTree
parseFileTests files = testGroup "Show . Parse <> id" $ map testFile files
  where
    testFile :: (FilePath, String) -> TestTree
    testFile (fp, content) = testCase fp $
      assertEqual ("show . parse <> id for: " ++ fp) content (show . parseFuspelString $ content)

simpleExpressionParserTests :: TestTree
simpleExpressionParserTests = testGroup "Simple Expression Parser Tests" $
    [ testCase "Int Positive" $
        parse simpleExpression "" "37" @?= (Right (SEInt 37))
    , testCase "Int Negative" $
        parse simpleExpression "" "-37" @?= (Right (SEInt (-37)))
    , testCase "Name" $
        parse simpleExpression "" "t_e_s_t173" @?= (Right (SEName "t_e_s_t173"))
    , testCase "List Colon" $
        parse simpleExpression "" "[1:[2:[]]]" @?= (Right (SEList [SEInt 1, SEInt 2] (Nothing)))
    , testCase "List Colon Tail" $
        parse simpleExpression "" "[1:[2:f]]" @?= (Right (SEList [SEInt 1, SEInt 2] (Just (SEName "f"))))
    , testCase "List Comma" $
        parse simpleExpression "" "[1,2:[]]" @?= (Right (SEList [SEInt 1, SEInt 2] (Nothing)))
    , testCase "List Comma Tail" $
        parse simpleExpression "" "[1,2:f]" @?= (Right (SEList [SEInt 1, SEInt 2] (Just (SEName "f"))))
    , testCase "Tuple" $
        parse simpleExpression "" "(1, 2)" @?= (Right (SETuple (SEInt 1) (SEInt 2)))
    , testCase "Wildcard" $
        parse simpleExpression "" "_" @?= (Right SEWildCard)
    , testCase "Invalid Identifier" $
        case parse simpleExpression "" "1a" of
          s@(Right (SEName _)) -> assertFailure ("Expected Non-SEName when parsing \"1a\", but got: " ++ show s)
          _ -> return ()
    ]

expressionParserTests :: TestTree
expressionParserTests = testGroup "Expression Parser Tests" $
    [ testCase "Int Positive" $
        parse expression "" "37" @?= (Right (EInt 37))
    , testCase "Int Negative" $
        parse expression "" "-37" @?= (Right (EInt (-37)))
    , testCase "Name" $
        parse expression "" "t_e_s_t173" @?= (Right (EName "t_e_s_t173"))
    , testCase "List Colon" $
        parse expression "" "[1:[2:[]]]" @?= (Right (EList [EInt 1, EInt 2] (Nothing)))
    , testCase "List Colon Tail" $
        parse expression "" "[1:[2:f a]]" @?= (Right (EList [EInt 1, EInt 2] (Just (EApp (EName "f") (EName "a")))))
    , testCase "List Comma" $
        parse expression "" "[1,2:[]]" @?= (Right (EList [EInt 1, EInt 2] (Nothing)))
    , testCase "List Comma Tail" $
        parse expression "" "[1,2:f]" @?= (Right (EList [EInt 1, EInt 2] (Just (EName "f"))))
    , testCase "Tuple" $
        parse expression "" "(1, 2)" @?= (Right (ETuple (EInt 1) (EInt 2)))
    , testCase "Invalid Identifier" $
        case parse expression "" "1a" of
          s@(Right (EName _)) -> assertFailure ("Expected Non-SEName when parsing \"1a\", but got: " ++ show s)
          _ -> return ()
    , testCase "Nested Application" $
        parse expression "" "a (b (c (d (e (f g) h) i) j) k) l)" @?=
          Right (EApp (EApp (EName "a") (EApp (EApp (EName "b") (EApp (EApp (EName "c") (EApp (EApp (EName "d") (EApp (EApp (EName "e") (EApp (EName "f") (EName "g"))) (EName "h"))) (EName "i"))) (EName "j"))) (EName "k"))) (EName "l"))
    , testCase "Parenthesis" $
        parse expression "" "((((((((((((((1))))))))))))))" @?= Right (EInt 1)
    ]
