module Codegen
    ( genLLVM
    ) where

import Data.String
import Lang
import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Type

genLLVM :: Lang.CompilationUnit -> Module
genLLVM (CompilationUnit main _) =
  defaultModule { moduleName           = fromString "program"
                , moduleSourceFileName = fromString "program.ps"
                , moduleDefinitions    = [definition main]
                }

definition :: Lang.Main -> Definition
definition (Main scope) =
  GlobalDefinition $
    functionDefaults { name       = Name $ fromString "main"
                     , returnType = i32
                     }
