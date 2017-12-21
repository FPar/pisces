module Lexer
    ( colon
    , identifier
    , reserved
    , reservedOp
    , rightArrow
    , parens
    , naturalOrFloat
    , float
    , semi
    , whiteSpace
    , braces
    ) where

import           Text.Parsec
import           Text.Parsec.Language
import qualified Text.Parsec.Token as P

languageDef =
  emptyDef { P.commentStart    = "/*"
           , P.commentEnd      = "*/"
           , P.commentLine     = "//"
           , P.identStart      = letter
           , P.identLetter     = alphaNum
           , P.reservedNames   = [ "fn"
                                 , "return"
                                 ]
           , P.reservedOpNames = [ "+", "-", "*", "/", "="
                                 , "==", "!=", "<", ">"
                                 , "&&", "||", "!"
                                 ]
           }

lexer = P.makeTokenParser languageDef

symbol            = P.symbol            lexer

colon             = P.colon             lexer
identifier        = P.identifier        lexer
reserved          = P.reserved          lexer
reservedOp        = P.reservedOp        lexer
rightArrow        = symbol "->"
parens            = P.parens            lexer
naturalOrFloat    = P.naturalOrFloat    lexer
float             = P.float             lexer
semi              = P.semi              lexer
whiteSpace        = P.whiteSpace        lexer
braces            = P.braces            lexer