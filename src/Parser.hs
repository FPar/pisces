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
                                 ]
           , P.reservedOpNames = [ "+", "-", "*", "/", "="
                                 , "==", "!=", "<", ">"
                                 , "&&", "||", "!"
                                 ]
           }

lexer = P.makeTokenParser languageDef

identifier = P.identifier lexer
reserved   = P.reserved   lexer
reservedOp = P.reservedOp lexer
parens     = P.parens     lexer
integer    = P.integer    lexer
semi       = P.semi       lexer
whiteSpace = P.whiteSpace lexer

programParser :: Parsec s () Ast.Program
programParser =
  return $ Ast.Program (Ast.Main $ Ast.Scope []) []

parseProgram :: SourceName -> String -> Either ParseError Ast.Program
parseProgram = parse programParser
