module Fuspel.AST
  where

data Fuspel
  = Fuspel [Import] [Rewrite]
  deriving (Show)

data Import
  = Import Name
  deriving (Show)

data Rewrite
  = Rewrite Name [SimpleExpression] Expression
  deriving (Show)

type Name
  = String

data SimpleExpression
  = SEInt Int
  | SEName Name
  -- List consist of a list of heads, followed by an optional tail expression
  | SEList [SimpleExpression] (Maybe SimpleExpression)
  | SETuple SimpleExpression SimpleExpression
  | SEWildCard
  deriving (Show)

data Expression
  = EInt Int
  | EName Name
  -- List consist of a list of heads, followed by an optional tail expression
  | EList [Expression] (Maybe Expression)
  | ETuple Expression Expression
  | EApp Expression Expression
  | ECode Name
  deriving (Show)
