module Parser
    ( parseProgram
    ) where

import qualified Ast
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language
import qualified Text.Parsec.Token      as P

languageDef =
  emptyDef { P.commentStart    = "/*"
           , P.commentEnd      = "*/"
           , P.commentLine     = "//"
           , P.identStart      = letter
           , P.identLetter     = alphaNum
           , P.reservedNames   = [ "func"
                                 , "return"
                                 , "main"
                                 ]
           , P.reservedOpNames = [ "+", "-", "*", "/", "="
                                 , "==", "!=", "<", ">"
                                 , "&&", "||", "!"
                                 ]
           }

lexer = P.makeTokenParser languageDef

identifier        = P.identifier        lexer
reserved          = P.reserved          lexer
reservedOp        = P.reservedOp        lexer
parens            = P.parens            lexer
naturalOrFloat    = P.naturalOrFloat    lexer
float             = P.float             lexer
semi              = P.semi              lexer
whiteSpace        = P.whiteSpace        lexer
braces            = P.braces            lexer

atomic :: Parsec String () Ast.Atomic
atomic = do
  val <- naturalOrFloat
  case val of
    Left nat -> return $ Ast.Integer nat
    Right float -> return $ Ast.Float float

expression :: Parsec String () Ast.Expression
expression = do
  val <- atomic
  return $ Ast.Atomic val

returnStmt :: Parsec String () Ast.Statement
returnStmt = do
  reserved "return"
  expr <- expression
  return $ Ast.Return expr

statement :: Parsec String () Ast.Statement
statement = returnStmt

scope :: Parsec String () Ast.Scope
scope = do
  val <- braces $ many statement
  return $ Ast.Scope val

main :: Parsec String () Ast.Main
main = do
  reserved "func"
  reserved "main"
  parens whiteSpace
  val <- scope
  return $ Ast.Main val

program :: Parsec String () Ast.Program
program = do
  mainVal <- main
  return $ Ast.Program mainVal []

parseProgram :: SourceName -> String -> Either ParseError Ast.Program
parseProgram = parse program
