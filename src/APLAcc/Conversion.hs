module APLAcc.Conversion (convertProgram) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.List (sortBy, isPrefixOf)
import Data.Ord (comparing)

import qualified APLAcc.TAIL.AST as T
import qualified APLAcc.SimpleAcc.AST as A
import APLAcc.SimpleAcc.AST (Type(..), BType(..), Name(..), QName(..))
import APLAcc.SimpleAcc.ToHaskell () -- Show instances for SimpleAcc.AST

type ConvertError = String
type ConvertResult = Either ConvertError

type Env = Map.Map T.Ident A.Type

emptyEnv :: Env
emptyEnv = Map.empty

type Convert a = ReaderT Env (Except ConvertError) a

runConvert m env = runExcept $ runReaderT m env

convertProgram :: T.Program -> ConvertResult A.Program
convertProgram p = runConvert (convertStmt p) emptyEnv

tryTypeCast :: A.Type -- from
            -> A.Type -- to
            -> Maybe (A.Exp -> A.Exp)
tryTypeCast (Plain t1)   (Exp t2)        | t1 == t2 = Just $ A.constant . flip A.TypSig (Plain t1)
tryTypeCast (Plain t1)   (Acc r t2)      | t1 == t2 = liftM2 (.) (tryTypeCast (Exp t1) (Acc r t1)) (tryTypeCast (Plain t1) (Exp t1))

tryTypeCast (Exp t1)    (Acc 0 t2)       = liftM (A.unit .) $ tryTypeCast (Exp t1) (Exp t2)
tryTypeCast (Exp t1)    (Acc 1 t2)       = liftM (A.unitvec .) $ tryTypeCast (Exp t1) (Acc 0 t2)

tryTypeCast (Acc 0 t1)  (Plain t2)       = liftM (. A.the) $ tryTypeCast (Exp t1) (Plain t2)
tryTypeCast (Acc 0 t1)  (Exp t2)         | t1 == t2 = Just A.the
tryTypeCast (Acc 1 t1)  (Exp t2)         | t1 == t2 = Just A.first
tryTypeCast (Acc 0 t1)  (Acc 1 t2)       | t1 == t2 = Just A.unitvec
tryTypeCast (Acc 1 t1)  (Acc 0 t2)       = liftM (. A.first) $ tryTypeCast (Exp t1) (Acc 0 t2)
tryTypeCast (Acc 1 IntT) (ShapeT)        = Just cancelShape

tryTypeCast (IO_ t1) (IO_ t2) | t1 /= t2 =
  do f <- tryTypeCast t1 t2
     return $ bindRet (f $ A.Var $ UnQual $ Ident "x")
  where bindRet e2 e1 = A.bind e1 (A.Fn "x" t1 (A.ret e2))

tryTypeCast t1 t2 | t1 == t2 = Just $ id
tryTypeCast t1 t2 = Nothing

typeCast :: A.Type -- from
            -> A.Type -- to
            -> A.Exp
            -> Convert A.Exp
typeCast fromT toT e =
  case tryTypeCast fromT toT of
    Just f -> return $ f e
    Nothing -> throwError $ "cannot type cast " ++ show e ++ " from " ++ show fromT ++ " to " ++ show toT

cancelShape :: A.Exp -> A.Exp
cancelShape (A.App (Primitive (Ident "shape")) [e]) = A.App (Accelerate $ Ident "shape") [e]
cancelShape e = A.shFromVec e

cancelLift :: A.Type -> A.Exp -> (A.Type, A.Exp)
cancelLift (Exp t1) (A.App (Accelerate (Ident "constant")) [A.TypSig e (Plain t2)]) | t1 == t2 = (Plain t1, e)
cancelLift t e = (t, e)

allPlain :: [T.Exp] -> Convert Bool
allPlain (T.Var name : es) =
  do env <- ask
     case Map.lookup name env of
       Nothing -> case name of
                    "pi" -> return False
                    _    -> throwError $ name ++ " not found in env"
       Just (Plain _) -> allPlain es
       _ -> return False
allPlain (T.I _ : es) = allPlain es
allPlain (T.D _ : es) = allPlain es
allPlain (T.B _ : es) = allPlain es
allPlain (T.C _ : es) = allPlain es
allPlain [] = return True
allPlain _ = return False

isIOPrimitive "bench" = True
isIOPrimitive "nowi" = True
isIOPrimitive "mem" = True
isIOPrimitive "memScl" = True
isIOPrimitive "readFile" = True
isIOPrimitive "readIntVecFile" = True
isIOPrimitive "readDoubleVecFile" = True
isIOPrimitive name | isPrefixOf "pr" name = True
isIOPrimitive _ = False

convertStmt :: T.Exp -> Convert [A.Stmt]
convertStmt (T.Let x t1 e1@(T.Op opname _ _) e2) | isIOPrimitive opname = do
  t1' <- convertType t1
  e1' <- convertExp e1 (IO_ t1')
  stmts <- local (Map.insert x t1') $ convertStmt e2
  return $ A.Bind x t1' e1' : stmts

convertStmt (T.Let x t1 e1 e2) = do
  t1' <- convertType t1
  e1' <- convertExp e1 t1'
  -- If the e1 has been lifted to Exp then drop the lift and store as Plain
  let (t3, e3) = cancelLift t1' e1'
  stmts <- local (Map.insert x t3) $ convertStmt e2
  return $ A.LetStmt x t3 e3 : stmts

convertStmt e@(T.Op opname _ _) | isIOPrimitive opname = do
  e' <- convertExp e (IO_ (Acc 0 DoubleT))
  return [A.Return True $ e']

convertStmt e = do
  e' <- convertExp e (Acc 0 DoubleT)
  return [A.Return False $ e']


convertExp :: T.Exp -> A.Type -> Convert A.Exp
convertExp (T.Var "zilde") (Acc 1 _) = return $ A.Var $ Primitive $ Ident "zilde"
convertExp (T.Var name) t = do
  env <- ask
  case Map.lookup name env of
    Nothing -> case name of
                 "pi" -> return $ A.Var $ Prelude $ Ident "pi"
                 _    -> throwError $ name ++ " not found in env"
    Just t2 -> typeCast t2 t $ A.Var $ UnQual $ Ident name

convertExp (T.I i) t = typeCast (Plain IntT) t $ A.I i
convertExp (T.D d) t = typeCast (Plain DoubleT) t $ A.D d
convertExp (T.B b) t = typeCast (Plain BoolT) t $ A.B b
convertExp (T.C c) t = typeCast (Plain CharT) t $ A.C c
convertExp (T.Inf) t = typeCast (Plain DoubleT) t $ A.Var $ Primitive $ Ident "infinity"

convertExp (T.Neg e) t = do
  let t' = Exp $ A.baseType t
  e' <- convertExp e t'
  let (t2, e2) = cancelLift t' e'
  typeCast t2 t $ A.Neg e2

convertExp (T.Let x t1 e1 e2) t2 = do
  t1' <- convertType t1
  e1' <- convertExp e1 t1'
  -- If the e1 has been lifted to Exp then drop the lift and store as Plain
  let (t3, e3) = cancelLift t1' e1'
  e2' <- local (Map.insert x t3) $ convertExp e2 t2
  return $ A.Let x t3 e3 e2'

convertExp (T.Op name instDecl args) t = convertOp name instDecl args t

convertExp (T.Fn x t1 e) t2 = do
  t1' <- convertType t1
  e' <- local (Map.insert x t1') (convertExp e t2)
  return $ A.Fn x t1' e'

convertExp (T.Vc [T.Var x]) (Acc 1 t) = do
  e' <- convertExp (T.Var x) (Exp t)
  return $ A.App (A.Primitive $ A.Ident "unitvec") [A.unit e']

convertExp (T.Vc []) (Acc 1 t) =
  return $ A.Var $ Primitive $ Ident "zilde"

convertExp (T.Vc (e:es)) (Acc 1 t) = do
  isPlain <- allPlain (e:es)
  if isPlain
    then do es' <- mapM (`convertExp` Plain t) (e:es)
            return $ A.TypSig (A.use $ A.fromList (length es') (A.List es')) (Acc 1 t)
    else do e' <- convertExp e (Acc 1 t)
            es' <- convertExp (T.Vc es) (Acc 1 t)
            return $ cat e' es'
  where cat x y = A.App (Primitive $ Ident "catV") [x, y]

convertExp (T.Vc es) (Plain t) = do
  es' <- mapM (`convertExp` Plain t) es
  return $ A.List es'

convertExp e t = throwError $ "failed to convert exp " ++ show e ++ " to type " ++ show t

convertType :: T.Type -> Convert A.Type
convertType (T.ArrT t (T.R 0)) = return $ Exp t
convertType (T.ArrT t (T.R r)) = return $ Acc r t
convertType (T.VecT t (T.R len)) = return $ Acc 1 t
convertType (T.ST t _) = return $ Exp t
convertType (T.SVT t _) = return $ Acc 1 t
convertType _ = throwError "convertType - not implemented"

functions :: Map.Map String (Maybe T.InstDecl -> A.Type -> ([A.Exp] -> A.Exp, [T.Exp -> Convert A.Exp], A.Type))
functions = Map.fromList
  [ ( "addi",    \Nothing                    t -> binOp (symb "+")   IntT    t )
  , ( "subi",    \Nothing                    t -> binOp (symb "-")   IntT    t )
  , ( "muli",    \Nothing                    t -> binOp (symb "*")   IntT    t )
  , ( "mini",    \Nothing                    t -> binOp (prel "min") IntT    t )
  , ( "maxi",    \Nothing                    t -> binOp (prel "max") IntT    t )
  , ( "andi",    \Nothing                    t -> binOp (prim "andi") IntT   t )
  , ( "ori",     \Nothing                    t -> binOp (prim "ori") IntT    t )
  , ( "xori",    \Nothing                    t -> binOp (prim "xori") IntT   t )
  , ( "shri",    \Nothing                    t -> binOp (acc "shiftR") IntT  t )
  , ( "shli",    \Nothing                    t -> binOp (acc "shiftL") IntT  t )
  , ( "eqi",     \Nothing                    t -> cmpOp (accSymb "==*") IntT t )
  , ( "lti",     \Nothing                    t -> cmpOp (accSymb "<*") IntT t )
  , ( "gti",     \Nothing                    t -> cmpOp (accSymb ">*") IntT t )
  , ( "negi",    \Nothing                    t -> unaryOp (\[a] -> A.Neg a) IntT    IntT    t )
  , ( "absi",    \Nothing                    t -> unaryOp (prel "abs")      IntT    IntT    t )
  , ( "signi",   \Nothing                    t -> unaryOp (prel "signum")   IntT    IntT    t )
  , ( "resi",    \Nothing                    _ -> (prim "residue",  [expArg IntT, expArg IntT], Exp IntT) )

  , ( "addd",    \Nothing                    t -> binOp (symb "+")   DoubleT t )
  , ( "subd",    \Nothing                    t -> binOp (symb "-")   DoubleT t )
  , ( "muld",    \Nothing                    t -> binOp (symb "*")   DoubleT t )
  , ( "divd",    \Nothing                    t -> binOp (symb "/")   DoubleT t )
  , ( "powd",    \Nothing                    t -> binOp (symb "**")  DoubleT t )
  , ( "mind",    \Nothing                    t -> binOp (prel "min") DoubleT t )
  , ( "maxd",    \Nothing                    t -> binOp (prel "max") DoubleT t )
  , ( "eqd",     \Nothing                    t -> cmpOp (accSymb "==*") DoubleT t )
  , ( "gtd",     \Nothing                    t -> cmpOp (accSymb ">*") DoubleT t )
  , ( "negd",    \Nothing                    t -> unaryOp (\[a] -> A.Neg a) DoubleT DoubleT t )
  , ( "absd",    \Nothing                    t -> unaryOp (prel "abs")      DoubleT DoubleT t )
  , ( "ln",      \Nothing                    t -> unaryOp (prel "log")      DoubleT DoubleT t )
  , ( "expd",    \Nothing                    t -> unaryOp (prel "exp")      DoubleT DoubleT t )
  , ( "sin",     \Nothing                    t -> unaryOp (prel "sin")      DoubleT DoubleT t )
  , ( "cos",     \Nothing                    t -> unaryOp (prel "cos")      DoubleT DoubleT t )
  , ( "tan",     \Nothing                    t -> unaryOp (prel "tan")      DoubleT DoubleT t )

  , ( "eqc",     \Nothing                    t -> cmpOp (accSymb "==*") CharT t )

  , ( "eqb",     \Nothing                    t -> binOp (accSymb "==*")  BoolT   t )
  , ( "andb",    \Nothing                    t -> binOp (accSymb "&&*")  BoolT   t )
  , ( "orb",     \Nothing                    t -> binOp (accSymb "||*")  BoolT   t )
  , ( "xorb",    \Nothing                    t -> binOp (accSymb "/=*")  BoolT   t )
  , ( "notb",    \Nothing                    t -> binOp (acc "not") BoolT   t )

  , ( "i2d",     \Nothing                    t -> unaryOp (prim "i2d")      IntT    DoubleT t )
  , ( "b2i",     \Nothing                    t -> unaryOp (prim "b2i")      BoolT   IntT    t )
  , ( "floor",   \Nothing                    t -> unaryOp (acc "floor")     DoubleT IntT    t )
  , ( "ceil",    \Nothing                    t -> unaryOp (acc "ceiling")   DoubleT IntT    t )
  , ( "signd",   \Nothing                    t -> unaryOp (prim "signd")    DoubleT IntT    t )

  , ( "each",    \(Just ([t1, t2], [r]))     _ -> (prim "each",     [funcArg (Exp t1) (Exp t2), accArg r t1], Acc r t2) )
  , ( "eachV",   \(Just ([t1, t2], [_]))     _ -> (prim "eachV",    [funcArg (Exp t1) (Exp t2), accArg 1 t1], Acc 1 t2) )
  , ( "reduce",  \(Just ([t], [r]))          _ -> (prim "reduce",   [funcArg (Exp t) (Exp t), expArg t, accArg (r+1) t], Acc r t) )
  , ( "cat",     \(Just ([t], [r]))          _ -> (prim "cat",      [accArg r t, accArg r t], Acc r t) )
  , ( "catV",    \(Just ([t], [_, _]))       _ -> (prim "catV",     [accArg 1 t, accArg 1 t], Acc 1 t) )
  , ( "iota",    \Nothing                    t -> (prim "iota",     [expArg IntT], Acc 1 (A.baseType t)) )
  , ( "iotaV",   \Nothing                    _ -> (prim "iotaV",    [expArg IntT], Acc 1 IntT) )
  , ( "drop",    \(Just ([t], [r]))          _ -> (prim "drop",     [expArg IntT, accArg r t], Acc r t) )
  , ( "dropV",   \(Just ([t], [_]))          _ -> (prim "dropV",    [expArg IntT, accArg 1 t], Acc 1 t) )
  -- FIXME: take should take a default element
  , ( "take",    \(Just ([t], [r]))          _ -> (prim "take",     [expArg IntT, accArg r t], Acc r t) )
  , ( "takeV",   \(Just ([t], [_]))          _ -> (prim "takeV",    [expArg IntT, accArg 1 t], Acc 1 t) )
  , ( "shape",   \(Just ([t], [r]))          _ -> (prim "shape",    [accArg r t], Acc 1 IntT) )
  , ( "shapeV",  \Nothing                    _ -> (prim "shapeV",   [accArg 1 IntT], Exp IntT) )
  -- FIXME: APLT currently uses the reshape name for reshape0
  , ( "reshape", \(Just ([t], [r1, r2]))     _ -> (prim "reshape0", [shapeArg, accArg r1 t], Acc r2 t) )
  , ( "reverse", \(Just ([t], [r]))          _ -> (prim "reverse",  [accArg r t], Acc r t) )
  , ( "cons",    \(Just ([t], [r]))          _ -> (prim "cons",     [accArg r t, accArg (r+1) t], Acc (r+1) t) )
  , ( "consV",   \(Just ([t], [_]))          _ -> (prim "consV",    [expArg t, accArg 1 t], Acc 1 t) )
  , ( "snoc",    \(Just ([t], [r]))          _ -> (prim "snoc",     [accArg (r+1) t, accArg r t], Acc (r+1) t) )
  , ( "snocV",   \(Just ([t], [_]))         rt -> case rt of
                                                    ShapeT -> (prim "snocSh",   [shapeArg, expArg IntT], ShapeT)
                                                    _      -> (prim "snocV",    [accArg 1 t, expArg t], Acc 1 t) )
  , ( "zipWith", \(Just ([t1, t2, t3], [r])) _ -> (prim "zipWith",  [funcArg (Exp t1) (Exp t3), accArg r t1, accArg r t2], Acc r t3) )
  , ( "rotate",  \(Just ([t], [r]))          _ -> (prim "rotate",   [expArg IntT, accArg r t], Acc r t) )
  , ( "rotateV", \(Just ([t], [r]))          _ -> (prim "rotateV",  [expArg IntT, accArg 1 t], Acc 1 t) )
  , ( "vrotate", \(Just ([t], [r]))          _ -> (prim "vrotate",  [expArg IntT, accArg r t], Acc r t) )
  , ( "vrotateV",\(Just ([t], [_]))          _ -> (prim "rotateV",  [expArg IntT, accArg 1 t], Acc 1 t) )
  , ( "transp",  \(Just ([t], [r]))          _ -> (prim "transp",   [accArg r t], Acc r t) )
  , ( "transp2", \(Just ([t], [r]))          _ -> (prim "transp2",  [transp2Arg, accArg r t], Acc r t) )
  -- FIXME: first should take a default element
  , ( "first",   \(Just ([t], [r]))          _ -> (prim "first",    [accArg r t], Exp t) )
  , ( "firstV",  \Nothing                    _ -> (prim "firstV",   [accArg 1 IntT], Exp IntT) )
  , ( "bench",   \(Just ([t], []))           _ -> (bench,           [funcArg (Acc 0 t) (Acc 0 t), plainArg IntT, accArg 0 t], IO_ (Acc 0 t)) )
  , ( "power",   \(Just ([t], [r]))          _ -> (power,           [funcArg (Acc r t) (Acc r t), plainArg IntT, accArg r t], Acc r t) )
  , ( "powerScl",\(Just ([t], []))           _ -> (power,           [funcArg (Acc 0 t) (Acc 0 t), plainArg IntT, accArg 0 t], Acc 0 t) )
  , ( "condScl", \(Just ([t], []))           _ -> (prim "condScl",  [funcArg (Acc 0 t) (Acc 0 t), expArg BoolT, accArg 0 t], Acc 0 t) )
  , ( "rav",     \(Just ([t], [r]))          _ -> (acc "flatten",   [accArg r t], Acc 1 t) )
  , ( "mem",     \Nothing                    t -> case t of
                                                    IO_ t' -> (mem, [flip convertExp t'], IO_ t')
                                                    _      -> (run, [flip convertExp t], t) )
  , ( "memScl",  \Nothing                    t -> case t of
                                                    IO_ t' -> (memScl, [flip convertExp (Exp (A.baseType t'))], IO_ (Exp (A.baseType t')))
                                                    _      -> (run, [flip convertExp t], t) )

  , ( "nowi",              \Nothing          _ -> (prim "now",               [plainArg IntT],  IO_ (Exp IntT)) )
  , ( "readFile",          \Nothing          _ -> (prim "readCharVecFile",   [plainArg CharT], IO_ (Acc 1 CharT)) )
  , ( "readIntVecFile",    \Nothing          _ -> (prim "readIntVecFile",    [plainArg CharT], IO_ (Acc 1 IntT)) )
  , ( "readDoubleVecFile", \Nothing          _ -> (prim "readDoubleVecFile", [plainArg CharT], IO_ (Acc 1 DoubleT)) )
  ]
  where symb = A.InfixApp . Prelude . Symbol
        accSymb = A.InfixApp . Accelerate . Symbol
        acc = A.App . Accelerate . Ident
        prim = A.App . Primitive . Ident
        prel = A.App . Prelude . Ident

        unaryOp f bty retbty (Plain _) = (f, [plainArg bty], Plain retbty)
        unaryOp f bty retbty _         = (f, [expArg bty],   Exp retbty)

        binOp f bty (Plain _) = (f, [plainArg bty, plainArg bty], Plain bty)
        binOp f bty _         = (f, [expArg bty,   expArg bty],   Exp bty)

        cmpOp f bty (Plain _) = (f, [plainArg bty, plainArg bty], Plain BoolT)
        cmpOp f bty _         = (f, [expArg bty,   expArg bty],   Exp BoolT)

        bench args = A.App (Primitive $ Ident "bench") (A.Var (Backend $ Ident "run1") : A.Var (Backend $ Ident "run"): args)
        power args = A.App (Primitive $ Ident "power") (A.Var (Backend $ Ident "run1") : A.Var (Backend $ Ident "run"): args)
        mem args = A.App (Primitive $ Ident "mem") (A.Var (Backend $ Ident "run") : args)
        memScl args = A.App (Primitive $ Ident "memScl") (A.Var (Backend $ Ident "run") : args)

        run = A.use . A.App (Backend $ Ident "run")

        plainArg :: A.BType -> T.Exp -> Convert A.Exp
        plainArg t = flip convertExp (Plain t)

        expArg :: A.BType -> T.Exp -> Convert A.Exp
        expArg t = flip convertExp (Exp t)

        accArg :: Integer -> A.BType -> T.Exp -> Convert A.Exp
        accArg n t = flip convertExp (Acc n t)

        transp2Arg :: T.Exp -> Convert A.Exp
        transp2Arg e = do perm <- intList e
                          return $ A.Tuple [A.IdxFn perm, A.IdxFn $ permInverse perm]
          where unwrapInt (T.I i) = return i
                unwrapInt e = throwError $ "expected int in argument to transp2, got " ++ show e
                intList (T.Vc es) = mapM unwrapInt es
                intList e = throwError $ "expected list in argument to transp2, got " ++ show e
                permInverse = map fst . sortBy (comparing snd) . zip [1..]

        shapeArg :: T.Exp -> Convert A.Exp
        shapeArg (T.Vc es) = do
          es' <- mapM (flip convertExp (Exp IntT)) es
          return $ A.lift $ A.InfixApp (Accelerate $ Symbol ":.") ((A.Var $ Accelerate $ Ident "Z") : es')
        shapeArg e = convertExp e (ShapeT)

        funcArg :: A.Type -> A.Type -> T.Exp -> Convert A.Exp
        funcArg (Exp IntT) _ (T.Var "i2d") = return $ A.Var $ Primitive $ Ident "i2d"
        funcArg (Exp BoolT) _ (T.Var "b2i") = return $ A.Var $ Primitive $ Ident "b2i"
        funcArg (Exp DoubleT) _ (T.Var "floor") = return $ A.Var $ Accelerate $ Ident "floor"
        funcArg (Exp DoubleT) _ (T.Var "ceil") = return $ A.Var $ Accelerate $ Ident "ceiling"

        funcArg (Exp IntT) _ (T.Var "addi") = return $ A.Var $ Prelude $ Symbol "+"
        funcArg (Exp IntT) _ (T.Var "subi") = return $ A.Var $ Prelude $ Symbol "-"
        funcArg (Exp IntT) _ (T.Var "muli") = return $ A.Var $ Prelude $ Symbol "*"
        funcArg (Exp IntT) _ (T.Var "mini") = return $ A.Var $ Prelude $ Ident "min"
        funcArg (Exp IntT) _ (T.Var "maxi") = return $ A.Var $ Prelude $ Ident "max"
        funcArg (Exp IntT) _ (T.Var "resi") = return $ A.Var $ Primitive $ Ident "residue"
        funcArg (Exp IntT) _ (T.Var "signi") = return $ A.Var $ Prelude $ Ident "signum"
        funcArg (Exp IntT) _ (T.Var "andi") = return $ A.Var $ Primitive $ Ident "andi"
        funcArg (Exp IntT) _ (T.Var "xori") = return $ A.Var $ Primitive $ Ident "xori"
        funcArg (Exp IntT) _ (T.Var "shri") = return $ A.Var $ Accelerate $ Ident "shiftR"
        funcArg (Exp IntT) _ (T.Var "shli") = return $ A.Var $ Accelerate $ Ident "shiftL"
        funcArg (Exp IntT) _ (T.Var "gti") = return $ A.Var $ Accelerate $ Symbol ">*"
        funcArg (Exp IntT) _ (T.Var "lti") = return $ A.Var $ Accelerate $ Symbol "<*"
        funcArg (Exp IntT) _ (T.Var "ltei") = return $ A.Var $ Accelerate $ Symbol "<=*"

        funcArg (Exp DoubleT) _ (T.Var "addd") = return $ A.Var $ Prelude $ Symbol "+"
        funcArg (Exp DoubleT) _ (T.Var "subd") = return $ A.Var $ Prelude $ Symbol "-"
        funcArg (Exp DoubleT) _ (T.Var "muld") = return $ A.Var $ Prelude $ Symbol "*"
        funcArg (Exp DoubleT) _ (T.Var "divd") = return $ A.Var $ Prelude $ Symbol "/"
        funcArg (Exp DoubleT) _ (T.Var "mind") = return $ A.Var $ Prelude $ Ident "min"
        funcArg (Exp DoubleT) _ (T.Var "maxd") = return $ A.Var $ Prelude $ Ident "max"
        funcArg (Exp DoubleT) _ (T.Var "abs") = return $ A.Var $ Prelude $ Ident "abs"
        funcArg (Exp DoubleT) _ (T.Var "log") = return $ A.Var $ Prelude $ Ident "log"
        funcArg (Exp DoubleT) _ (T.Var "exp") = return $ A.Var $ Prelude $ Ident "exp"
        funcArg (Exp DoubleT) _ (T.Var "sin") = return $ A.Var $ Prelude $ Ident "sin"
        funcArg (Exp DoubleT) _ (T.Var "cos") = return $ A.Var $ Prelude $ Ident "cos"
        funcArg (Exp DoubleT) _ (T.Var "tan") = return $ A.Var $ Prelude $ Ident "tan"

        funcArg (Exp CharT) _ (T.Var "eqc") = return $ A.Var $ Accelerate $ Symbol "==*"

        funcArg (Exp BoolT) _ (T.Var "eqb") = return $ A.Var $ Accelerate $ Symbol "==*"
        funcArg (Exp BoolT) _ (T.Var "andb") = return $ A.Var $ Accelerate $ Symbol "&&*"
        funcArg (Exp BoolT) _ (T.Var "orb")  = return $ A.Var $ Accelerate $ Symbol "||*"
        funcArg (Exp BoolT) _ (T.Var "xorb") = return $ A.Var $ Accelerate $ Symbol "/=*"
        funcArg (Exp BoolT) _ (T.Var "notb") = return $ A.Var $ Accelerate $ Ident "not"
        funcArg t1 t2 (T.Fn x t3 e) = do
          e' <- local (Map.insert x t1) (convertExp e t2)
          return $ A.Fn x t1 e'
        funcArg t1 t2 name = throwError $ show name ++ " not implemented as function for " ++ show t1 ++ " -> " ++ show t2


convertOp :: T.Ident -> Maybe T.InstDecl -> [T.Exp] -> A.Type -> Convert A.Exp
convertOp name@('p':'r':_) _ [arg] (IO_ t) =
  do e' <- convertExp arg t
     return $ A.App (Primitive $ Ident name) [A.Var (Backend $ Ident "run"), e']
convertOp ('p':'r':_) _ [arg] t = convertExp arg t >>= \x -> return x
convertOp name inst args t =
  case Map.lookup name functions of
    Just f  -> do let (g, argTyps, retTyp) = f inst t
                  e <- liftM g (convertArgs argTyps args)
                  typeCast retTyp t e
    Nothing -> throwError $ name ++ "{" ++ show inst ++ "} not implemented"


convertArgs :: [T.Exp -> Convert A.Exp] -> [T.Exp] -> Convert [A.Exp]
convertArgs = zipWithM ($)
