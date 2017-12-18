module Ast where

type Identifier = String

data Atomic = Integer Integer
            | Float Float

data Expression = Atomic

data Statement = Assignment Identifier Expression
               | Return Expression

newtype Scope = Scope [Expression]

data Prototype = Prototype Identifier [Identifier]
data Func = Func Prototype Scope
newtype Main = Main Scope

data Program = Program Main [Func]
