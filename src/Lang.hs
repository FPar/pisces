module Lang where

type Identifier = String

newtype CompilationUnit = CompilationUnit [Function]
  deriving Show

data Function = Function Identifier [ParameterDeclaration] ReturnType Block
  deriving Show

type ReturnType = Maybe Type

newtype Block = Block [Statement]
  deriving Show
data Statement = Declaration Identifier Type (Maybe Expression)
               | Assignment Identifier Expression
               | Return Expression
               | If Expression Block (Maybe Block)
               | Unary UnaryOp Expression
  deriving Show

data ParameterDeclaration = ParameterDeclaration Identifier Type
  deriving Show
data Type = I64 | F64
  deriving Show

data Expression = Atomic Atomic
                | Variable String
                | Math MathOp Expression Expression
                | Comparison BoolOp Expression Expression
                | Negate Expression
                | Invocation String [Expression]
  deriving Show
data UnaryOp = Increment | Decrement
  deriving Show
data MathOp = Addition | Subtraction | Multiplication | Division | Modulo
  deriving Show
data BoolOp = Eq | Ne | Lt | Gt | Le | Ge
  deriving Show
data Atomic = Integer Integer
            | Float Double
  deriving Show
