module APLAcc.SimpleAcc.AST (
  module APLAcc.SimpleAcc.AST,
  T.BType(..)  -- reexport
) where

import qualified APLAcc.TAIL.AST as T

data Type
  = Exp T.BType         -- Exp t
  | Acc Integer T.BType -- Acc (Array n t)
  | Plain T.BType
  | ShapeT
  | IO_ Type            -- IO monad
  deriving (Eq)

baseType :: Type -> T.BType
baseType (Exp t) = t
baseType (Acc _ t) = t
baseType (Plain t) = t

data Name
  = Ident T.Ident
  | Symbol String

data QName
  = UnQual Name
  | Prelude Name
  | Accelerate Name
  | Primitive Name
  | Backend Name

data Stmt
  = LetStmt T.Ident Type Exp
  | Bind T.Ident Type Exp
  | Return Bool Exp

data Exp
  = Var QName
  | I Integer
  | D Double
  | B Bool
  | C Char
  | Shape [Integer]
  | Neg Exp
  | TypSig Exp Type
  | List [Exp]
  | Tuple [Exp]
  | InfixApp QName [Exp]       -- x1 `op` x2 `op` …
  | App QName [Exp]            -- op x1 x2 …
  | Let T.Ident Type Exp Exp    -- let x = e1 :: t in e2
  | Fn T.Ident Type Exp         -- \x -> e
  | IdxFn [Integer]             -- \(Z :. a1 :. a2 :. a3 -> Z :. a3 :. a1 :. a2)

type Program = [Stmt]

the x  = App (Accelerate $ Ident "the") [x]
unit x = App (Accelerate $ Ident "unit") [x]
constant x = App (Accelerate $ Ident "constant") [x]
lift x = App (Accelerate $ Ident "lift") [x]
unlift x = App (Accelerate $ Ident "unlift") [x]
i2d x  = App (Primitive  $ Ident "i2d") [x]
fromList n x = App (Accelerate $ Ident "fromList") [Shape [fromIntegral n], x]
use x  = App (Accelerate $ Ident "use") [x]
fromInt x = App (Prelude $ Ident "fromIntegral") [x]
unitvec x = App (Primitive $ Ident "unitvec") [x]
shFromVec x = App (Primitive $ Ident "shFromVec") [x]
first x = App (Primitive $ Ident "first") [x]
ret x = App (Prelude $ Ident "return") [x]
