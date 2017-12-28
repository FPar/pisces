module Lexer
    ( braces
    , colon
    , float
    , identifier
    , natural
    , naturalOrFloat
    , parens
    , reserved
    , reservedOp
    , rightArrow
    , semi
    , symbol
    , whiteSpace
    ) where

import           Text.Parsec
import           Text.Parsec.Language
import qualified Text.Parsec.Token    as P

languageDef =
  emptyDef { P.commentStart    = "/*"
           , P.commentEnd      = "*/"
           , P.commentLine     = "//"
           , P.identStart      = letter
           , P.identLetter     = alphaNum
           , P.reservedNames   = [ "fn"
                                 , "return"
                                 , "var"
                                 ]
           , P.reservedOpNames = [ "+", "-", "*", "/", "="
                                 , "==", "!=", "<", ">"
                                 , "&&", "||", "!"
                                 ]
           }

lexer = P.makeTokenParser languageDef

symbol            = P.symbol            lexer

braces            = P.braces            lexer
colon             = P.colon             lexer
float             = P.float             lexer
identifier        = P.identifier        lexer
natural           = P.natural           lexer
naturalOrFloat    = P.naturalOrFloat    lexer
parens            = P.parens            lexer
reserved          = P.reserved          lexer
reservedOp        = P.reservedOp        lexer
rightArrow        = symbol "->"
semi              = P.semi              lexer
whiteSpace        = P.whiteSpace        lexer
