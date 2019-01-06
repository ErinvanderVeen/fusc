module Fuspel.AST
  where

import Data.List (intersperse, groupBy)

data Fuspel
  = Fuspel [Import] [Rewrite]
  deriving(Eq)

data Import
  = Import Name
  deriving(Eq)

data Rewrite
  = Rewrite Name [SimpleExpression] Expression
  deriving(Eq)

type Name
  = String

data SimpleExpression
  = SEInt Int
  | SEName Name
  -- List consist of a list of heads, followed by an optional tail expression
  | SEList [SimpleExpression] (Maybe SimpleExpression)
  | SETuple SimpleExpression SimpleExpression
  | SEWildCard
  deriving(Eq)

data Expression
  = EInt Int
  | EName Name
  -- List consist of a list of heads, followed by an optional tail expression
  | EList [Expression] (Maybe Expression)
  | ETuple Expression Expression
  | EApp Expression Expression
  | ECode Name
  deriving(Eq)

instance Show Fuspel
  where
    show (Fuspel imports rules) = (unlines . map show) imports ++ "\n" ++ showRules rules ++ "\n"
      where
        showRules = (concat . intersperse "\n\n") . map (concat . intersperse "\n") . map (map show) . groupBy nameEq
          where
            nameEq (Rewrite n1 _ _) (Rewrite n2 _ _) = n1 == n2

instance Show Import
  where
    show (Import s) = "import " ++ s ++ ";"

instance Show Rewrite
  where
    show (Rewrite name args expr) = name ++ " " ++ unwords (map show args) ++ " = " ++ show expr ++ ";"

instance Show SimpleExpression
  where
    show (SEInt i) = show i
    show (SEName s) = s
    show (SEList l Nothing) = show l
    show (SEList l (Just t)) = "[" ++ (concat . intersperse "," . map show) l ++ ":" ++ show t ++ "]"
    show (SETuple l r) = "(" ++ show l ++ ", " ++ show r ++ ")"
    show SEWildCard = "_"

instance Show Expression
  where
    show (EInt i) = show i
    show (EName n) = n
    show (EList l Nothing) = show l
    show (EList l (Just t)) = "[" ++ (concat . intersperse "," . map show) l ++ ":" ++ show t ++ "]"
    show (ETuple l r) = "(" ++ show l ++ ", " ++ show r ++ ")"
    show (EApp f a@(EApp _ _)) = show f ++ " (" ++ show a ++ ")"
    show (EApp f a) = show f ++ " " ++ show a
    show (ECode n) = "code " ++ n
