-- | Contains low level parsers for string tokenization.
module Lexer
    ( braces
    , colon
    , commaSep
    , identifier
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
           , P.reservedNames   = [ "fn", "return"
                                 , "var"
                                 , "if", "else"
                                 ]
           , P.reservedOpNames = [ "+", "-", "*", "/", "="
                                 , "++", "--"
                                 , "==", "!=", "<", ">"
                                 , "&&", "||", "!"
                                 ]
           }

lexer = P.makeTokenParser languageDef

-- | Parses the given symbol.
symbol            = P.symbol            lexer

-- | Wraps the parser in braces.
braces            = P.braces            lexer
-- | Parses ":".
colon             = P.colon             lexer
-- | Parses any number of the provided parser seperated by commas.
commaSep          = P.commaSep          lexer
-- | Parses a valid identifier.
identifier        = P.identifier        lexer
-- | Parses an integer of float literal.
naturalOrFloat    = P.naturalOrFloat    lexer
-- | Wraps the parser in parentheses.
parens            = P.parens            lexer
-- | Parses a reserved identifier.
reserved          = P.reserved          lexer
-- | Parser a reserver operator.
reservedOp        = P.reservedOp        lexer
-- | Parses "->".
rightArrow        = symbol "->"
-- | Parses ";".
semi              = P.semi              lexer
-- | Parses all sorts of white space.
whiteSpace        = P.whiteSpace        lexer
