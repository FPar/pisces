module ParserSpec (spec) where

import Data.List
import Data.Maybe

import Test.Hspec

import Lang
import Parser

spec :: Spec
spec =
  describe "parseUnit" $ do
    let main = "main"
    it "parses an empty function" $
      parseUnit "" (genFunction main [] Nothing []) `shouldBe` Right (buildUnit [buildFunction main [] Nothing []])
    it "parses a return number statement" $
      parseUnit "" (genFunction main [] (Just "i64") ["return 12"]) `shouldBe` Right (buildUnit [buildFunction main [] (Just I64) [Return $ Atomic $ Integer 12]])
    it "parses a function incrementing a variable and returning it." $
      let txtStmts = [ "var a: i64 = 12"
                     , "++a"
                     , "return a"
                     ]
          langStmts = [ Declaration "a" I64 $ Just $ Atomic $ Integer 12
                      , Unary Increment $ Variable "a"
                      , Return $ Variable "a"
                      ]
      in
      parseUnit "" (genFunction main [] (Just "i64") txtStmts) `shouldBe` Right (buildUnit [buildFunction main [] (Just I64) langStmts])
    it "parses a function call with parameters that returns a value." $
      let txtA = genFunction "a" ["x: i64"] (Just "i64") ["return x"]
          langA = buildFunction "a" [ParameterDeclaration "x" I64] (Just I64) [Return $ Variable "x"]
          txtMain = genFunction main [] (Just "i64") ["return a(12)"]
          langMain = buildFunction main [] (Just I64) [Return $ Invocation "a" [Atomic $ Integer 12]]
      in
      parseUnit "" (txtA ++ " " ++ txtMain) `shouldBe` Right (buildUnit [langA, langMain])
    it "parses an if statement" $
      let txtMax = genFunction "max" ["a: i64", "b: i64"] (Just "i64") ["if (a > b) { return a; } else { return b; }"]
          langMax = buildFunction "max" [ParameterDeclaration "a" I64, ParameterDeclaration "b" I64] (Just I64)
                      [If (Comparison Gt (Variable "a") (Variable "b")) (Block [Return $ Variable "a"]) (Just $ Block [Return $ Variable "b"])]
      in
      parseUnit "" txtMax `shouldBe` Right (buildUnit [langMax])

genFunction :: String -> [String] -> Maybe String -> [String] -> String
genFunction name params retty statements =
  "fn " ++ name ++ " (" ++ commaSep params ++ ")" ++ genRetty retty ++ " {\n" ++ semiSep statements ++ "\n}"

genRetty :: Maybe String -> String
genRetty = fromMaybe "" . fmap ((++) " -> ")

buildUnit :: [Function] -> CompilationUnit
buildUnit = CompilationUnit

buildFunction :: String -> [ParameterDeclaration] -> ReturnType -> [Statement] -> Function
buildFunction name params retty statements =
  Function name params retty $ Block statements
  
commaSep :: [String] -> String
commaSep = intercalate ", "

semiSep :: [String] -> String
semiSep = concat . map (\x -> if isPrefixOf "if" x then id x else x ++ ";\n")

