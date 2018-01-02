-- | Module which contains the AST for the pisces language.
module Lang where

-- | Name of a function of variable.
type Identifier = String

-- | A file is represented by a compilation unit.
newtype CompilationUnit = CompilationUnit [Function]
  deriving (Show, Eq)

-- | A function declaration and definition.
data Function = Function Identifier [ParameterDeclaration] ReturnType Block
  deriving (Show, Eq)

-- | Return type of a function. Nothing means void.
type ReturnType = Maybe Type

-- | A sequence of statements.
newtype Block = Block [Statement]
  deriving (Show, Eq)
-- | A complete statement.
data Statement = Declaration Identifier Type (Maybe Expression)
               | Assignment Identifier Expression
               | Return Expression
               | If Expression Block (Maybe Block)
               | Unary UnaryOp Expression
  deriving (Show, Eq)

-- | Parameter declaration for a function declaration.
data ParameterDeclaration = ParameterDeclaration Identifier Type
  deriving (Show, Eq)
-- | Built-in types.
data Type = I64 | F64
  deriving (Show, Eq)

-- | An expression which can be evaluated for use within a statement.
data Expression = Atomic Atomic
                | Variable String
                | Math MathOp Expression Expression
                | Comparison BoolOp Expression Expression
                | Negate Expression
                | Invocation String [Expression]
  deriving (Show, Eq)
-- | Unary operators.
data UnaryOp = Increment | Decrement
  deriving (Show, Eq)
-- | Math operators.
data MathOp = Addition | Subtraction | Multiplication | Division | Modulo
  deriving (Show, Eq)
-- | Boolean operators.
data BoolOp = Eq | Ne | Lt | Gt | Le | Ge
  deriving (Show, Eq)
-- | Atomics within expressions.
data Atomic = Integer Integer
            | Float Double
  deriving (Show, Eq)
