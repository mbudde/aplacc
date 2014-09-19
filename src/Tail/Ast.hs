module Tail.Ast where

type Ident = String

data Rank
  = R Integer
  | Rv String
  | Radd Rank Rank
  deriving (Show)

data BType = IntT | DoubleT | Btyv Ident
  deriving (Show)

data Type
  = ArrT BType Rank
  | ShT Rank
  | SiT Rank
  | ViT Rank
  | FunT Type Type
  deriving (Show)

scalar :: BType -> Type
scalar b = ArrT b (R 0)

int = scalar IntT
double = scalar DoubleT


data Shape = Sh [Integer]
  deriving (Show)

data Exp
  = Var Ident
  | I Integer
  | D Double
  | Let Ident Type Exp Exp -- Type, calculate while parsing?
  | Op String [Exp] -- Type
  | Fn Ident Type Exp -- Type
  | Vc [Exp] -- Type
  deriving (Show)

type Program = Exp
