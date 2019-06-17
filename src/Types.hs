module Types where

type TagValues = [(String,String)]
data Tag
  = OpenTag String TagValues
  | CloseTag String
  | SimpleTag String TagValues
  deriving (Eq, Show)

data Element = Leaf String
  | Node String TagValues [Element]
  deriving(Eq, Show)
