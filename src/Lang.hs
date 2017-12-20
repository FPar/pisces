module Lang where

type Identifier = String

data Atomic = Integer Integer
            | Float Double
  deriving Show

data Expression = Atomic Atomic
  deriving Show

data Statement = Assignment Identifier Expression
               | Return Expression
  deriving Show

newtype Scope = Scope [Statement]
  deriving Show

data Prototype = Prototype Identifier [Identifier]
  deriving Show
data Func = Func Prototype Scope
  deriving Show
newtype Main = Main Scope
  deriving Show

data CompilationUnit = CompilationUnit Main [Func]
  deriving Show
