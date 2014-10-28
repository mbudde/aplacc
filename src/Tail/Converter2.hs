module Tail.Converter2 where

import Control.Monad.Reader
import Data.Maybe (fromJust)
import qualified Data.Map as Map

import Tail.Ast as T
import Tail.SimpleAccAst as A
import Tail.Parser (parseFile)


type Env = Map.Map Ident A.Type

emptyEnv :: Env
emptyEnv = Map.fromList [("zilde", Acc 1 (Btyv "e"))]

type Convert a = Reader Env a

runConvert = runReader

whenT :: T.BType -> T.BType -> (a -> a) -> Convert a -> Convert a
whenT t1 t2 f m | t1 == t2 = liftM f m
whenT _  _  _ m = m


convertFile :: String -> IO ()
convertFile file = do ast <- parseFile file
                      putStrLn $ show $ convertProgram ast

convertProgram :: T.Program -> A.Program
convertProgram p = runConvert (convertExp p (Acc 0 DoubleT)) emptyEnv

typeCast :: A.Type -- from
         -> A.Type -- to
         -> A.Exp -> A.Exp
typeCast (Exp t1)    (Acc 0 t2)    = unit . typeCast (Exp t1) (Exp t2)
typeCast (Acc 0 t1)  (Exp t2)      = typeCast (Exp t1) (Exp t2) . the
typeCast (Acc 0 t1)  (Acc 0 t2)    = unit . typeCast (Exp t1) (Exp t2) . the
typeCast (Exp IntT)  (Exp DoubleT) = i2d
typeCast (Exp t1)    (Exp t2)      | t1 == t2 = id
typeCast (Acc r1 t1) (Acc r2 t2)   | t1 == t2 && r1 == r2 = id
typeCast (Acc r1 (Btyv _)) (Acc r2 _) = id
typeCast (Acc r1 _) (Acc r2 (Btyv _)) = id
typeCast t1 t2 = \e -> error $ "cannot type cast " ++ show e ++ " from " ++ show t1 ++ " to " ++ show t2

convertExp :: T.Exp -> A.Type -> Convert A.Exp
convertExp (T.Var name) t = do
  env <- ask
  return $ case Map.lookup name env of
    Nothing           -> error $ name ++ " not found in env"
    Just t2 | t == t2 -> A.Var $ UnQual $ Ident name
    Just t2           -> typeCast t2 t $ A.Var $ UnQual $ Ident name

convertExp (T.I i) t = return $ typeCast (Exp IntT) t $ TypSig (A.I i) (Exp IntT)
convertExp (T.D d) t = return $ typeCast (Exp DoubleT) t $ TypSig (A.D d) (Exp DoubleT)
convertExp (T.Inf) _ = undefined

convertExp (T.Neg e) (Exp t) = liftM A.Neg $ convertExp e (Exp t)
convertExp (T.Neg e) (Acc 0 t) = liftM unit $ liftM A.Neg $ convertExp e (Exp t)
convertExp (T.Neg _) _ = error "Cannot convert neg"

convertExp (T.Let x t1 e1 e2) t2 = do
  let t1' = convertType t1
  e1' <- convertExp e1 t1'
  e2' <- local (Map.insert x t1') $ convertExp e2 t2
  return $ A.Let x t1' e1' e2'

convertExp (T.Op name instDecl args) t = do
  (e, t2) <- convertOp name instDecl args t
  return $ typeCast t2 t e

convertExp (T.Fn x t1 e) t2 = do
  let t1' = convertType t1
  e' <- local (Map.insert x t1') (convertExp e t2)
  return $ A.Fn x t1' e'

convertExp (T.Vc es) (Acc 1 t) = do
  es' <- mapM (flip convertExp (Exp t)) es
  return $ TypSig (use $ fromList (length es') (List es')) (Acc 1 t)

convertExp e t = error $ show e ++ show t

convertType :: T.Type -> A.Type
convertType (ArrT t (R 0)) = Exp t
convertType (ArrT t (R r)) = Acc r t
convertType (ShT (R 1)) = Exp IntT
convertType (ShT (R r)) = Acc 1 IntT
convertType (SiT _) = Exp IntT
convertType (ViT _) = Exp IntT
convertType _ = error "convertType - not implemented"

functions :: Map.Map String (Maybe InstDecl -> A.Type -> ([A.Exp] -> A.Exp, [T.Exp -> Convert A.Exp], A.Type))
functions = Map.fromList
  [ ( "addi",    \Nothing _ -> (symb "+", [expArg IntT, expArg IntT], Exp IntT) )
  , ( "i2d",     \Nothing _ -> (prim "i2d", [expArg IntT], Exp DoubleT) )
  , ( "reduce",  \(Just ([t], [r])) _ -> (prim "reduce", [funcArg $ Exp t, expArg t,accArg (r+1) t], Acc r t) )
  , ( "each",    \(Just ([t1, t2], [r])) _ -> (prim "each", [funcArg $ Exp t1, accArg r t1], Acc r t2) )
  , ( "catSh",   \Nothing _ -> (prim "catSh", [accArg 1 IntT, accArg 1 IntT], Acc 1 IntT) )
  , ( "cat",     \(Just ([t], [r])) _ -> (prim "cat", [accArg r t, accArg r t], Acc r t) )
  , ( "iotaSh",  \Nothing _ -> (prim "iotaSh", [expArg IntT], Acc 1 IntT) )
  , ( "maxi",    \Nothing _ -> (prel "max", [expArg IntT, expArg IntT], Exp IntT) )
  , ( "drop",    \(Just ([t], [r])) _ -> (prim "drop", [expArg t, accArg r t], Acc r t) )
  , ( "zipWith", \(Just ([t1, t2, t3], [r])) _ -> (prim "zipWith", [funcArg $ Exp t1, accArg r t1, accArg r t2], Acc r t3) )
  , ( "shape",   \(Just ([t], [r])) _ -> (prim "shape", [accArg r t], Acc 1 IntT) )
  ]
  where symb = A.InfixApp . Prelude . Symbol
        prim = A.App . Primitive . Ident
        prel = A.App . Prelude . Ident

convertOp :: T.Ident -> Maybe T.InstDecl -> [T.Exp] -> A.Type -> Convert (A.Exp, A.Type)
convertOp name inst args t =
  case Map.lookup name functions of
    Just f  -> do let (g, argTyps, retTyp) = f inst t
                  e <- liftM g (convertArgs argTyps args)
                  return (e, retTyp)
    Nothing -> error $ name ++ "{" ++ show inst ++ "} not implemented"

funcArg :: A.Type -> T.Exp -> Convert A.Exp
funcArg (Exp IntT) (T.Var "addi") = return $ A.Var $ Prelude $ Symbol "+"
funcArg (Exp IntT) (T.Var "subi") = return $ A.Var $ Prelude $ Symbol "-"
funcArg (Exp IntT) (T.Var "muli") = return $ A.Var $ Prelude $ Symbol "*"
funcArg (Exp IntT) (T.Var "mini") = error "mini not implemented"
funcArg (Exp IntT) (T.Var "maxi") = return $ A.Var $ Prelude $ Ident "max"
funcArg (Exp DoubleT) (T.Var "addd") = return $ A.Var $ Prelude $ Symbol "+"
funcArg (Exp DoubleT) (T.Var "subd") = return $ A.Var $ Prelude $ Symbol "-"
funcArg (Exp DoubleT) (T.Var "muld") = return $ A.Var $ Prelude $ Symbol "*"
funcArg (Exp DoubleT) (T.Var "mind") = error "mind not implemented"
funcArg (Exp DoubleT) (T.Var "maxd") = error "mind not implemented"
funcArg t e@(T.Fn{}) = convertExp e t
funcArg t name = error $ show name ++ " not implemented as function for " ++ show t

expArg :: A.BType -> T.Exp -> Convert A.Exp
expArg t = flip convertExp (Exp t)

accArg :: Integer -> A.BType -> T.Exp -> Convert A.Exp
accArg n t = flip convertExp (Acc n t)

convertArgs :: [T.Exp -> Convert A.Exp] -> [T.Exp] -> Convert [A.Exp]
convertArgs fs es = sequence $ zipWith ($) fs es
