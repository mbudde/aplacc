module Tail.Converter where

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc (noLoc)
import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified Tail.Ast as T
import Tail.Parser (parseFile)


convertFile :: String -> IO ()
convertFile file = do ast <- parseFile file
                      putStrLn $ prettyPrint $ convertProgram ast 

prettyPrintProgram :: T.Program -> String
prettyPrintProgram = prettyPrint . convertProgram

convertProgram :: T.Program -> Module
convertProgram p =
  Module noLoc (ModuleName "Main") [] Nothing Nothing imports [progSig, prog, main]
  where imports = [ ImportDecl { importLoc       = noLoc
                               , importModule    = ModuleName "Data.Array.Accelerate"
                               , importQualified = True
                               , importSrc       = False
                               , importPkg       = Nothing
                               , importAs        = Just $ ModuleName "Acc"
                               , importSpecs     = Nothing }
                  , ImportDecl { importLoc       = noLoc
                               , importModule    = ModuleName "Data.Array.Accelerate.Interpreter"
                               , importQualified = False
                               , importSrc       = False
                               , importPkg       = Nothing
                               , importAs        = Nothing
                               , importSpecs     = Nothing }
                  , ImportDecl { importLoc       = noLoc
                               , importModule    = ModuleName "Tail.Primitives"
                               , importQualified = False
                               , importSrc       = False
                               , importPkg       = Nothing
                               , importAs        = Nothing
                               , importSpecs     = Nothing }]
        -- Assume result is always scalar double for now
        progSig = TypeSig noLoc [Ident "program"] $ acc (scalar double)
        prog = FunBind $
          [Match noLoc (Ident "program") [] Nothing
                 (UnGuardedRhs $ convertExp p) (BDecls [])]
        main = FunBind $
          [Match noLoc (Ident "main") [] Nothing
                 (UnGuardedRhs $ mainBody) (BDecls [])]
        mainBody = App (Var $ UnQual $ Ident "print") $
          App (Var $ UnQual $ Ident "run") (Var $ UnQual $ Ident "program")

qualAcc :: Name -> QName
qualAcc name = Qual (ModuleName "Acc") name

acc    = TyApp $ TyCon $ qualAcc $ Ident "Acc"
scalar = TyApp $ TyCon $ qualAcc $ Ident "Scalar"
vector = TyApp $ TyCon $ qualAcc $ Ident "Vector"
array d = TyApp (TyApp (TyCon $ qualAcc $ Ident "Array") d)
dim n  = TyCon $ qualAcc $ Ident $ "DIM" ++ show n
int    = TyCon $ UnQual $ Ident "Int"
double = TyCon $ UnQual $ Ident "Double"

{- qualTailPrim :: Name -> QName -}
{- qualTailPrim name = Qual (ModuleName "Acc") name -}

accVar :: String -> Exp
accVar name = Var $ qualAcc $ Ident name

convertName :: String -> QName
convertName "addi" = UnQual $ Symbol "+"
convertName "maxi" = UnQual $ Ident "max"
convertName s = UnQual $ Ident s


convertExp :: T.Exp -> Exp
convertExp (T.Var name) = Var $ convertName name
convertExp (T.I i) = Lit $ Int i
convertExp (T.D d) = Lit $ Frac $ toRational d
convertExp (T.Let ident typ e1 e2) =
  Let (BDecls [ TypeSig noLoc [Ident ident] $ TyApp (TyCon $ qualAcc $ Ident "Acc") (convertType typ)
              , PatBind noLoc (PVar $ Ident ident)
                        Nothing
                        (UnGuardedRhs $ convertExp e1)
                        (BDecls []) {- where binding -} ])
      (convertExp e2)
convertExp (T.Op name es) = convertOp name es
convertExp (T.Vc es) =
  App (accVar "use") $
    foldl App (accVar "fromList") [snocList $ length es, List $ map convertExp es]
convertExp (T.Fn ident typ e) =
  Lambda noLoc [PVar $ Ident ident] (convertExp e)

snocList :: Int -> Exp
snocList n = convertInfixAppExp (QVarOp $ qualAcc $ Symbol ":.")
                                [ Var $ qualAcc $ Ident "Z"
                                , Lit $ Int $ toInteger n]

convertOp "addi"   = convertInfixApp (QVarOp $ UnQual $ Symbol "+")
convertOp "cat"    = convertInfixApp (QVarOp $ qualAcc $ Symbol "++")
convertOp "reduce" = convertApp (accVar "fold")
convertOp "each"   = convertApp (accVar "map")
convertOp "drop"   = convertApp (accVar "drop")
convertOp "catSh"  = convertOp "cat"
convertOp "iotaSh" = convertOp "iota"
convertOp name     = convertApp (Var $ convertName name)

convertApp :: Exp -> [T.Exp] -> Exp
convertApp fun args = foldl App fun $ map convertExp args

convertInfixAppExp :: QOp -> [Exp] -> Exp
convertInfixAppExp fun (x1:x2:xs) = foldl (\e1 e2 -> InfixApp e1 fun e2)
                                          (InfixApp x1 fun x2) xs
convertInfixAppExp (QVarOp name) [] = Var name
convertInfixAppExp (QConOp name) [] = Con name
convertInfixAppExp _ _ = error "Cant convert infix operation with 1 argument"

convertInfixApp :: QOp -> [T.Exp] -> Exp
convertInfixApp fun xs = convertInfixAppExp fun $ map convertExp xs

convertType :: T.Type -> Type
convertType (T.ArrT btyp (T.R n)) = array (dim n) (convertBType btyp)
convertType (T.ShT _) = vector int

convertBType :: T.BType -> Type
convertBType T.IntT = int
convertBType T.DoubleT = double
convertBType _ = undefined


