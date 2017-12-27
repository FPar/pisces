module Lang where

type Identifier = String

newtype CompilationUnit = CompilationUnit [Function]
  deriving Show

data Function = Function Identifier [VariableDeclaration] ReturnType Block
  deriving Show

type ReturnType = Maybe Type

newtype Block = Block [Statement]
  deriving Show
data Statement = Assignment Identifier Expression
               | Return Expression
  deriving Show

data VariableDeclaration = VariableDeclaration Identifier Type
  deriving Show
data Type = I64 | F64
  deriving Show

data Expression = Atomic Atomic
                | Math MathOp Expression Expression
                | Comparison BoolOp Expression Expression
                | Negate Expression
                | Invocation Function [Expression]
  deriving Show
data MathOp = Addition | Subtraction | Multiplication | Division | Modulo
  deriving Show
data BoolOp = Eq | Ne | Lt | Gt | Le | Ge
  deriving Show
data Atomic = Integer Integer
            | Float Double
  deriving Show
