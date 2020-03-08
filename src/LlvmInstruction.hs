
module LlvmInstruction where

import System.Environment

import Data.Word
import Data.String
import Data.List
import Data.List as DL
import Data.Function
import qualified Data.Map as Map
import Control.Monad.Except
import Data.ByteString.Char8 as BS
import Data.ByteString.Short
import Control.Monad.State
import Control.Applicative
import LLVM.Context
import LLVM.Module
import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST as AST
import qualified LLVM.AST.Operand as ASTO
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Instruction
import qualified Data.ByteString as B

import Syntax as S
import TokenLexer
import ToolsLLVM
import Linker

binops = Map.fromList [
      ("+", FAdd)
    , ("-", FSub)
    , ("*", FMul)
    , ("/", FDiv)
    --, ("<", lt)
  ]

execTmp :: [Expr] -> Word -> (Word, [Named Instruction])
execTmp [] ctr = (ctr, [])
execTmp exprs ctr = execExpression exprs ctr

execExpression :: [Expr] -> Word -> (Word, [Named Instruction])
execExpression [] ctr = (ctr, [])
execExpression (expr:exprs) ctr = do
  let list = execBody expr ctr
  case (exprs) of 
    (x:xs) -> do
      let counter = (fst list) + 1
      let tmp = (execExpression exprs counter)
      (fst tmp, (snd list ++ (snd tmp)))
    _ -> do
      let counter = (fst list)
      let tmp = (execExpression exprs counter)
      (fst tmp, (snd list ++ (snd tmp)))

execBody :: Expr -> Word -> (Word, [Named Instruction])
execBody (S.Call name args) ctr = (ctr, [UnName ctr := fcall (allocArray name) (checkTypeTab args)])
execBody (BinOp opType op1 op2) ctr = do
      case isOp op2 of
        True -> do
          let new = ctr + 1
          let list = (execBody op2 new)
          let res = (UnName (fst $ list))
          let retOp = returnFun opType (checkType op1) (LocalReference double res)
          ((fst list + 1), (snd list) ++ [(UnName (fst list + 1) := retOp)])
        _ -> (ctr, [UnName ctr := returnFun opType (checkType op1) (checkType op2)])

getParams :: [Expr] -> [Parameter]
getParams [] = []
getParams (param:params) = (Parameter double (Name (convertToBS (exprVal param))) []) : getParams params

execFunction :: Expr -> Definition
execFunction (S.Function func_name parameters bodyContent ret) = GlobalDefinition functionDefaults
  { name = Name (convertToBS func_name)
  , parameters =
      ( getParams parameters
      , False )
  , returnType = double
  , basicBlocks = [body]
  }
  where
    list = (execTmp bodyContent 0)
    body = BasicBlock
      (Name (convertToBS "entry"))
      (snd list)
      (LLVM.AST.Instruction.Do $ Ret (Just (LocalReference double (UnName (fst list)))) [])

module_ :: [Definition] -> AST.Module
module_ defs = defaultModule
  { moduleName = (convertToBS "basic")
    , moduleDefinitions = defs
  }

toLLVM :: AST.Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
  args <- getArgs
  let srcFile = DL.head args
  B.writeFile (srcFile `modifExtension` ".ll") llvm

convertToBSTab :: [String] -> [ShortByteString]
convertToBSTab [] = []
convertToBSTab (str:tab) = [convertToBS str] ++ convertToBSTab tab

doLLVMCode :: ([Expr], [Expr]) -> [Definition] -> IO ()
doLLVMCode ([], []) defs = toLLVM $ module_ defs
doLLVMCode ([], main) defs = doLLVMCode (main, []) defs
doLLVMCode ((expr:exprs), main) defs = do
  let res = execFunction expr
  doLLVMCode (exprs, main) (defs ++ [res])
