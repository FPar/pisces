module Parser
    ( parseUnit
    ) where

import qualified Lang
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

atomic :: Parsec String () Lang.Atomic
atomic = do
  val <- naturalOrFloat
  case val of
    Left nat -> return $ Lang.Integer nat
    Right float -> return $ Lang.Float float

expression :: Parsec String () Lang.Expression
expression = do
  val <- atomic
  return $ Lang.Atomic val

returnStmt :: Parsec String () Lang.Statement
returnStmt = do
  reserved "return"
  expr <- expression
  return $ Lang.Return expr

statement :: Parsec String () Lang.Statement
statement = do
  val <- returnStmt
  semi
  return val

scope :: Parsec String () Lang.Scope
scope = do
  val <- braces $ many statement
  return $ Lang.Scope val

main :: Parsec String () Lang.Main
main = do
  reserved "func"
  reserved "main"
  parens whiteSpace
  val <- scope
  return $ Lang.Main val

compilationUnit :: Parsec String () Lang.CompilationUnit
compilationUnit = do
  mainVal <- main
  return $ Lang.CompilationUnit mainVal []

parseUnit :: SourceName -> String -> Either ParseError Lang.CompilationUnit
parseUnit = parse compilationUnit
