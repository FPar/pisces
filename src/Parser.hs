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
function =
  reserved "fn" >>
    Lang.Function <$> identifier <*> parens (many variableDeclaration) >>=
      \func -> rightArrow >> func <$> langType <*> block

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
