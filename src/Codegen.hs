module Codegen
    ( genLLVM
    ) where

import Data.String
import Lang
import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Type

genLLVM :: Lang.CompilationUnit -> Module
genLLVM (CompilationUnit functions) =
  defaultModule { moduleName           = fromString "program"
                , moduleDefinitions    = map definition functions
                }

definition :: Lang.Function -> Definition
definition (Lang.Function name parameters returnType definition) =
  GlobalDefinition $
    functionDefaults { name       = Name $ fromString name
                     , returnType = i32
                     }
