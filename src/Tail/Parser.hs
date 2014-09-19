module Tail.Parser where

import Control.Monad (liftM, liftM2)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Text.Parsec hiding (Empty)
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Pos
import qualified Text.Parsec.Token as Token

import Tail.Ast


tailDef = Token.LanguageDef {
                Token.commentStart     = "(*"
              , Token.commentEnd       = "*)"
              , Token.commentLine      = ""
              , Token.nestedComments   = False
              , Token.identStart       = letter
              , Token.identLetter      = alphaNum <|> char '_'
              , Token.opStart          = oneOf ""
              , Token.opLetter         = oneOf ""
              , Token.reservedOpNames  = []
              , Token.reservedNames    = [ "let", "in", "int", "double", "fn" ]
              , Token.caseSensitive    = True
  }

lexer = Token.makeTokenParser tailDef

t_identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
stringlit  = Token.stringLiteral lexer
parens     = Token.parens     lexer
brackets   = Token.brackets   lexer
integer    = Token.integer    lexer
semi       = Token.semi       lexer
comma      = Token.comma      lexer
colon      = Token.colon      lexer
symbol     = Token.symbol     lexer
whitespace = Token.whiteSpace lexer
decimal    = Token.decimal    lexer
float      = Token.float      lexer
lexeme     = Token.lexeme     lexer

withPrefix :: Parser a -> Parser b -> (a -> b -> b) -> Parser b
withPrefix pre p f =
  do x <- optionMaybe pre
     y <- p
     return $ case x of
       Just x' -> f x' y
       Nothing -> y

program :: Parser Program
program =
  do whitespace
     prog <- expr
     eof
     return prog

-----------------
-- Expression

expr :: Parser Exp
expr = opExpr
   <|> valueExpr
   <|> letExpr
   <|> fnExpr
   <|> (liftM Var t_identifier)
   <?> "expression"

valueExpr :: Parser Exp
valueExpr = (liftM D $ neg (lexeme float))
        <|> (liftM I $ neg (lexeme decimal))
        <|> arrayValue
  where neg p = try (withPrefix (char '~') p (\_ y -> -y))

arrayValue :: Parser Exp
arrayValue = liftM Vc $ brackets (sepBy expr comma)

letExpr :: Parser Exp
letExpr =
  do reserved "let"
     (ident, typ) <- typedIdent
     symbol "="
     e1 <- expr
     reserved "in"
     e2 <- expr
     return $ Let ident typ e1 e2

opExpr :: Parser Exp
opExpr =
  do ident <- try $ do { i <- t_identifier; lookAhead $ char '('; return i }
     args <- parens $ sepBy expr comma
     return $ Op ident args

fnExpr :: Parser Exp
fnExpr =
  do reserved "fn"
     (ident, typ) <- typedIdent
     symbol "=>"
     e <- expr
     return $ Fn ident typ e

typedIdent :: Parser (Ident, Type)
typedIdent =
  do ident <- t_identifier
     colon
     typ <- typeExpr
     return (ident, typ)

------------------
-- Types

typeExpr :: Parser Type
typeExpr =
      arrayType
  <|> shapeType
  <?> "type"

arrayType :: Parser Type
arrayType = liftM2 ArrT (brackets basicType) rank

shapeType :: Parser Type
shapeType = shape "Sh" ShT
        <|> shape "Si" SiT
        <|> shape "Vi" ViT
        <?> "shape type"
  where shape name con = try (symbol name) >> liftM con (parens rank)

rank :: Parser Rank
rank = (liftM R (lexeme decimal))
   <|> (liftM Rv t_identifier)
   <?> "rank"

basicType :: Parser BType
basicType = (reserved "int" >> return IntT)
        <|> (reserved "double" >> return DoubleT)
        <?> "basic type"

-------------------
-- Debug functions

parseString :: Parser a -> String -> a
parseString parser str =
  case parse parser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO ()
parseFile file =
  do str <- readFile file
     case parse program file str of
       Left e  -> error $ show e
       Right r -> putStrLn $ show r
