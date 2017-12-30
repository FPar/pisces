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
compilationUnit = whiteSpace >> CompilationUnit <$> many function

function :: Parsec String () Function
function = do
  reserved "fn"
  ident <- identifier
  parameters <- parens (commaSep parameterDeclaration)
  retType <- optionMaybe (rightArrow >> langType)
  definition <- block
  return $ Function ident parameters retType definition

block :: Parsec String () Block
block = do
  definition <- braces (do
    statements <- many statement
    ifStmt <- optionMaybe ifStmt
    case ifStmt of
      Nothing -> return statements
      Just stmt -> return $ statements ++ [stmt])
  return $ Block definition

statement :: Parsec String () Statement
statement = returnStmt <* semi
        <|> varStmt <* semi
        <|> unaryStmt <* semi
        <|> assignmentStmt <* semi

returnStmt :: Parsec String () Statement
returnStmt = reserved "return" >> Return <$> expression

varStmt :: Parsec String () Statement
varStmt = do
  reserved "var"
  name <- identifier
  colon
  t <- langType
  let decl = Declaration name t
  expr <- (reservedOp "=" >> Just <$> expression) <|> return Nothing
  return $ decl expr

assignmentStmt :: Parsec String () Statement
assignmentStmt = do
  target <- identifier
  reservedOp "="
  expr <- expression
  return $ Assignment target expr

unaryStmt :: Parsec String () Statement
unaryStmt = (reservedOp "++" >> identifier >>= \ident -> return $ Unary Increment (Variable ident))
        <|> (reservedOp "--" >> identifier >>= \ident -> return $ Unary Decrement (Variable ident))

ifStmt :: Parsec String () Statement
ifStmt = do
  reserved "if"
  condition <- parens expression
  scope <- block
  elseBlock <- optionMaybe (reserved "else" >> block)
  return $ If condition scope elseBlock

parameterDeclaration :: Parsec String () ParameterDeclaration
parameterDeclaration = ParameterDeclaration <$> identifier >>= \ var -> colon >> var <$> langType

langType :: Parsec String () Type
langType =
  try (symbol "i64" >> return I64) <|>
  try (symbol "f64" >> return F64) <?>
  "type."

expression :: Parsec String () Expression
expression = buildExpressionParser table term <?> "expression"

term :: Parsec String () Expression
term = parens expression <|> atomicOrVariableOrInvocation <?> "simple expression"

table = [ [ binary "*" (Math Multiplication) AssocLeft
          , binary "/" (Math Division) AssocLeft
          , binary "%" (Math Modulo) AssocLeft
          ]
        , [ binary "+" (Math Addition) AssocLeft
          , binary "-" (Math Subtraction) AssocLeft
          ]
        , [ binary "==" (Comparison Eq) AssocLeft
          , binary "!=" (Comparison Ne) AssocLeft
          , binary "<" (Comparison Lt) AssocLeft
          , binary ">" (Comparison Gt) AssocLeft
          , binary "<=" (Comparison Le) AssocLeft
          , binary ">=" (Comparison Ge) AssocLeft
          ]
        ]

binary :: String -> (a -> a -> a) -> Assoc -> Operator String u Identity a
binary name fun = Infix (reservedOp name >> return fun)
prefix :: String -> (a -> a) -> Operator String u Identity a
prefix name fun = Prefix (reservedOp name >> return fun)
postfix :: String -> (a -> a) -> Operator String u Identity a
postfix name fun = Postfix (reservedOp name >> return fun)

atomicOrVariableOrInvocation :: Parsec String () Expression
atomicOrVariableOrInvocation =
  Atomic <$> atomic <|>
  do
    ident <- identifier
    parameters <- optionMaybe $ parens $ commaSep expression
    case parameters of
      Nothing -> return $ Variable ident
      Just parameters -> return $ Invocation ident parameters

atomic :: Parsec String () Atomic
atomic = naturalOrFloat >>=
  \ case
    Left nat -> return $ Integer nat
    Right float -> return $ Float float
