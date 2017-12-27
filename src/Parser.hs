{-# LANGUAGE LambdaCase #-}

module Parser
    ( parseUnit
    ) where

import Data.Functor.Identity
import Lang
import Lexer
import Text.Parsec
import Text.Parsec.Expr

parseUnit :: SourceName -> String -> Either ParseError CompilationUnit
parseUnit = parse compilationUnit

compilationUnit :: Parsec String () CompilationUnit
compilationUnit = CompilationUnit <$> many function

function :: Parsec String () Function
function =
  reserved "fn" >>
    Function <$> identifier <*> parens (many variableDeclaration) >>=
      \func -> rightArrow >> func <$> langType <*> block

block :: Parsec String () Block
block = Block <$> braces (many statement)

statement :: Parsec String () Statement
statement = returnStmt >>= \stmt -> semi >> return stmt

returnStmt :: Parsec String () Statement
returnStmt = reserved "return" >> Return <$> expression

variableDeclaration :: Parsec String () VariableDeclaration
variableDeclaration = VariableDeclaration <$> identifier >>= \var -> colon >> var <$> langType

langType :: Parsec String () Type
langType = identifier >>=
  \case
    "i64" -> return I64
    "f64" -> return F64
    _ -> fail "Invalid type."

expression :: Parsec String () Expression
expression = buildExpressionParser table term <?> "expression"

term :: Parsec String () Expression
term = parens expression <|> (Atomic <$> atomic) <?> "simple expression"

table = [ [ binary "*" (Math Multiplication) AssocLeft
          , binary "/" (Math Division) AssocLeft
          , binary "%" (Math Modulo) AssocLeft
          ]
        , [ binary "+" (Math Addition) AssocLeft
          , binary "-" (Math Subtraction) AssocLeft
          ]
        ]

binary :: String -> (a -> a -> a) -> Assoc -> Operator String u Identity a
binary name fun = Infix (reservedOp name >> return fun)
prefix :: String -> (a -> a) -> Operator String u Identity a
prefix  name fun = Prefix (reservedOp name >> return fun)
postfix :: String -> (a -> a) -> Operator String u Identity a
postfix name fun = Postfix (reservedOp name >> return fun)

atomic :: Parsec String () Atomic
atomic = naturalOrFloat >>=
  \case
    Left nat -> return $ Integer nat
    Right float -> return $ Float float
