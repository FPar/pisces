module Lang where

type Identifier = String

newtype CompilationUnit = CompilationUnit [Function]
  deriving Show

data Function = Function Identifier [VariableDeclaration] Type Block
  deriving Show

newtype Block = Block [Statement]
  deriving Show
data Statement = Assignment Identifier Expression
               | Return Expression
  deriving Show

data VariableDeclaration = VariableDeclaration Identifier Type
  deriving Show
newtype Type = BuiltinType Identifier
  deriving Show

newtype Expression = Atomic Atomic
  deriving Show
data Atomic = Integer Integer
            | Float Double
  deriving Show
