module Main where

import System.Environment

import Lib
import Syntax
import TokenLexer
import LlvmInstruction
import Linker

main :: IO ()
main = do
  args <- getArgs
  let srcFile = head args
  fileContent <- readFile srcFile
  let llir = srcFile `modifExtension` ".ll"
  let lexer = getTokens fileContent
  let parser = buildAST' lexer []
  print parser
  let defs = buildDefs parser ([], [])
  let res = (fst defs, [Function "main" [] (snd defs) (Var "Double")])
  print res
  doLLVMCode res []
  koakCompile llir "a.out"
