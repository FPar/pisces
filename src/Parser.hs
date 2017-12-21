module Parser
    ( parseUnit
    ) where

import qualified Lang
import Lexer
import Text.Parsec

parseUnit :: SourceName -> String -> Either ParseError Lang.CompilationUnit
parseUnit = parse compilationUnit

compilationUnit :: Parsec String () Lang.CompilationUnit
compilationUnit = Lang.CompilationUnit <$> many function

function :: Parsec String () Lang.Function
function = do
  reserved "func"
  name <- identifier
  parameters <- parens $ many variableDeclaration
  colon
  returnType <- langType
  definition <- block
  return $ Lang.Function name parameters returnType definition

block :: Parsec String () Lang.Block
block = Lang.Block <$> braces (many statement)

statement :: Parsec String () Lang.Statement
statement = returnStmt >>= \stmt -> semi >> return stmt

returnStmt :: Parsec String () Lang.Statement
returnStmt = reserved "return" >> Lang.Return <$> expression

variableDeclaration :: Parsec String () Lang.VariableDeclaration
variableDeclaration = Lang.VariableDeclaration <$> identifier >>= \var -> colon >> var <$> langType

langType :: Parsec String () Lang.Type
langType = Lang.BuiltinType <$> identifier

expression :: Parsec String () Lang.Expression
expression = Lang.Atomic <$> atomic

atomic :: Parsec String () Lang.Atomic
atomic = naturalOrFloat >>= \value ->
  case value of
    Left nat -> return $ Lang.Integer nat
    Right float -> return $ Lang.Float float
