import Test.Hspec
import Test.QuickCheck
import Control.Exception
import System.Environment

import TokenLexer
import Syntax

main :: IO ()
main = hspec $ do
  describe "\nLexer cases" $ do
    it "function add" $ do
      fileContent <- readFile $ "ressources/exampleFunc"
      getTokens fileContent `shouldBe` [TokKeyWord Def,TokIdentifier "foo",TokParenthesis Open,TokIdentifier "x",TokSpecialChar Colon,TokType Double,TokParenthesis Close,TokSpecialChar Colon,TokType Double,TokIdentifier "x",TokOperator Plus,TokLiteral (Floating 2.0),TokSpecialChar EndStatement]

    it "function minus" $ do
      fileContent <- readFile $ "ressources/exampleMinus"
      getTokens fileContent `shouldBe` [TokKeyWord Def,TokIdentifier "foo",TokParenthesis Open,TokIdentifier "x",TokSpecialChar Colon,TokType Double,TokParenthesis Close,TokSpecialChar Colon,TokType Double,TokIdentifier "x",TokOperator Minus,TokLiteral (Floating 1.0),TokSpecialChar EndStatement]

    it "two simple functions add" $ do
      fileContent <- readFile $ "ressources/exampleMultipleFunc"
      getTokens fileContent `shouldBe` [TokKeyWord Def,TokIdentifier "foo",TokParenthesis Open,TokIdentifier "x",TokSpecialChar Colon,TokType Double,TokParenthesis Close,TokSpecialChar Colon,TokType Double,TokIdentifier "x",TokOperator Plus,TokLiteral (Floating 1.0),TokSpecialChar EndStatement,TokKeyWord Def,TokIdentifier "bar",TokParenthesis Open,TokIdentifier "x",TokSpecialChar Colon,TokType Double,TokParenthesis Close,TokSpecialChar Colon,TokType Double,TokIdentifier "x",TokOperator Plus,TokLiteral (Floating 3.0),TokSpecialChar EndStatement]

    it "function add then assign" $ do
      fileContent <- readFile $ "ressources/exampleFuncAssign"
      getTokens fileContent `shouldBe` [TokKeyWord Def,TokIdentifier "foo",TokParenthesis Open,TokIdentifier "x",TokSpecialChar Colon,TokType Double,TokParenthesis Close,TokSpecialChar Colon,TokType Double,TokIdentifier "x",TokOperator Plus,TokLiteral (Floating 1.0),TokSpecialChar EndStatement,TokIdentifier "y",TokOperator Assignment,TokLiteral (Floating 5.0),TokSpecialChar EndStatement]

    it "function mult" $ do
      fileContent <- readFile $ "ressources/exampleMult"
      getTokens fileContent `shouldBe` [TokKeyWord Def,TokIdentifier "foo",TokParenthesis Open,TokIdentifier "x",TokSpecialChar Colon,TokType Double,TokParenthesis Close,TokSpecialChar Colon,TokType Double,TokIdentifier "x",TokOperator Mult,TokLiteral (Floating 1.0),TokSpecialChar EndStatement,TokIdentifier "y",TokOperator Assignment,TokLiteral (Floating 5.0),TokSpecialChar EndStatement]

    it "function multiple add" $ do
      fileContent <- readFile $ "ressources/exampleAddAdd"
      getTokens fileContent `shouldBe` [TokKeyWord Def,TokIdentifier "foo",TokParenthesis Open,TokIdentifier "x",TokSpecialChar Colon,TokType Double,TokParenthesis Close,TokSpecialChar Colon,TokType Double,TokIdentifier "x",TokOperator Plus,TokLiteral (Floating 1.0),TokOperator Plus,TokLiteral (Floating 2.0),TokSpecialChar EndStatement]

    it "function add mult" $ do
      fileContent <- readFile $ "ressources/exampleAddMult"
      getTokens fileContent `shouldBe` [TokKeyWord Def,TokIdentifier "foo",TokParenthesis Open,TokIdentifier "x",TokSpecialChar Colon,TokType Double,TokParenthesis Close,TokSpecialChar Colon,TokType Double,TokIdentifier "x",TokOperator Mult,TokLiteral (Floating 1.0),TokOperator Plus,TokLiteral (Floating 1.0),TokSpecialChar EndStatement]

    it "if basic case" $ do
      fileContent <- readFile $ "ressources/exampleIf"
      getTokens fileContent `shouldBe` [TokKeyWord TokenLexer.If,TokIdentifier "x",TokOperator Equal,TokLiteral (Floating 0.0),TokKeyWord TokenLexer.Then,TokIdentifier "x",TokOperator Assignment,TokLiteral (Floating 1.0),TokSpecialChar EndStatement]

    it "while basic case" $ do
      fileContent <- readFile $ "ressources/exampleSimpleWhile"
      getTokens fileContent `shouldBe` [TokIdentifier "y",TokOperator Assignment,TokLiteral (Floating 2.0),TokSpecialChar EndStatement,TokKeyWord TokenLexer.While,TokIdentifier "y",TokOperator Lesser,TokLiteral (Floating 10.0),TokKeyWord Do,TokIdentifier "y",TokOperator Assignment,TokIdentifier "y",TokOperator Mult,TokLiteral (Floating 2.0),TokSpecialChar EndStatement]

    it "function def, assignement, while, call with add as arg" $ do
      fileContent <- readFile $ "ressources/exampleWhile"
      getTokens fileContent `shouldBe` [TokKeyWord Def,TokIdentifier "test",TokParenthesis Open,TokIdentifier "x",TokSpecialChar Colon,TokType Double,TokParenthesis Close,TokSpecialChar Colon,TokType Double,TokIdentifier "x",TokOperator Minus,TokLiteral (Floating 1.0),TokSpecialChar EndStatement,TokIdentifier "y",TokOperator Assignment,TokLiteral (Floating 2.0),TokSpecialChar EndStatement,TokKeyWord TokenLexer.While,TokIdentifier "y",TokOperator Lesser,TokLiteral (Floating 10.0),TokKeyWord Do,TokIdentifier "y",TokOperator Assignment,TokIdentifier "y",TokOperator Mult,TokLiteral (Floating 2.0),TokSpecialChar EndStatement,TokIdentifier "test",TokParenthesis Open,TokIdentifier "y",TokOperator Plus,TokLiteral (Floating 3.0),TokParenthesis Close,TokSpecialChar EndStatement]


  describe "\nParser cases" $ do
    it "function add" $ do
      fileContent <- readFile $ "ressources/exampleFunc"
      buildAST' (getTokens fileContent) [] `shouldBe` [Function "foo" [Var "x"] [BinOp Plus (Var "x") (Lit (Floating 2.0))] (Var "Double")]
      
    it "function minus" $ do
      fileContent <- readFile $ "ressources/exampleMinus"
      buildAST' (getTokens fileContent) [] `shouldBe` [Function "foo" [Var "x"] [BinOp Minus (Var "x") (Lit (Floating 1.0))] (Var "Double")]

    it "two simple functions add" $ do
      fileContent <- readFile $ "ressources/exampleAddAdd"
      buildAST' (getTokens fileContent) [] `shouldBe` [Function "foo" [Var "x"] [BinOp Plus (Var "x") (BinOp Plus (Lit (Floating 1.0)) (Lit (Floating 2.0)))] (Var "Double")]
      
    it "function add then assign" $ do
      fileContent <- readFile $ "ressources/exampleFuncAssign"
      buildAST' (getTokens fileContent) [] `shouldBe` [Function "foo" [Var "x"] [BinOp Plus (Var "x") (Lit (Floating 1.0))] (Var "Double"),BinOp Assignment (Var "y") (Lit (Floating 5.0))]

    it "function mult" $ do
      fileContent <- readFile $ "ressources/exampleMult"
      buildAST' (getTokens fileContent) [] `shouldBe` [Function "foo" [Var "x"] [BinOp Mult (Var "x") (Lit (Floating 1.0))] (Var "Double"),BinOp Assignment (Var "y") (Lit (Floating 5.0))]

    it "function multiple add" $ do
      fileContent <- readFile $ "ressources/exampleAddAdd"
      buildAST' (getTokens fileContent) [] `shouldBe` [Function "foo" [Var "x"] [BinOp Plus (Var "x") (BinOp Plus (Lit (Floating 1.0)) (Lit (Floating 2.0)))] (Var "Double")]
      
    it "function add mult" $ do
      fileContent <- readFile $ "ressources/exampleAddMult"
      buildAST' (getTokens fileContent) [] `shouldBe` [Function "foo" [Var "x"] [BinOp Plus (BinOp Mult (Var "x") (Lit (Floating 1.0))) (Lit (Floating 1.0))] (Var "Double")]

    it "if basic case" $ do
      fileContent <- readFile $ "ressources/exampleIf"
      buildAST' (getTokens fileContent) [] `shouldBe` [Syntax.If (BinOp Equal (Var "x") (Lit (Floating 0.0))) (BinOp Assignment (Var "x") (Lit (Floating 1.0)))]

    it "while basic case" $ do
      fileContent <- readFile $ "ressources/exampleSimpleWhile"
      buildAST' (getTokens fileContent) [] `shouldBe` [BinOp Assignment (Var "y") (Lit (Floating 2.0)),Syntax.While (BinOp Lesser (Var "y") (Lit (Floating 10.0))) (BinOp Assignment (Var "y") (BinOp Mult (Var "y") (Lit (Floating 2.0))))]

    it "function def, assignement, while, call with add as arg" $ do
      fileContent <- readFile $ "ressources/exampleWhile"
      buildAST' (getTokens fileContent) [] `shouldBe` [Function "test" [Var "x"] [BinOp Minus (Var "x") (Lit (Floating 1.0))] (Var "Double"),BinOp Assignment (Var "y") (Lit (Floating 2.0)),Syntax.While (BinOp Lesser (Var "y") (Lit (Floating 10.0))) (BinOp Assignment (Var "y") (BinOp Mult (Var "y") (Lit (Floating 2.0)))),Call "test" [BinOp Plus (Var "y") (Lit (Floating 3.0))]]
