module ToolsLLVM where

import Data.List as DL
import Control.Monad.Except
import Data.ByteString.Char8 as BS
import Data.ByteString.Short
import Control.Monad.State
import LLVM.AST.Global
import LLVM.AST
import qualified LLVM.AST.AddrSpace as AS
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Instruction
import qualified Data.ByteString as B
import qualified LLVM.AST.Attribute as A
import Syntax as S
import TokenLexer

double :: LLVM.AST.Type
double = FloatingPointType DoubleFP

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs [] = []
toArgs (x:xs) = ([(x, [])] ++ toArgs xs)

convertToBS :: String -> ShortByteString
convertToBS name = (toShort(BS.pack(name)))

fsub :: Operand -> Operand -> Instruction
fsub a b = FSub noFastMathFlags a b []

fadd :: Operand -> Operand -> Instruction
fadd a b = FAdd noFastMathFlags a b []

fmul :: Operand -> Operand -> Instruction
fmul a b = FMul noFastMathFlags a b []

fdiv :: Operand -> Operand -> Instruction
fdiv a b = FDiv noFastMathFlags a b []

alloca :: LLVM.AST.Type -> Instruction
alloca ty = Alloca ty Nothing 0 []

fcall :: Operand -> [Operand] -> Instruction
fcall name args = LLVM.AST.Call Nothing CC.C [] (Right name) (toArgs args) [] []

allocArray :: String -> Operand
allocArray str = LocalReference (PointerType double (AS.AddrSpace (toEnum (DL.length str + 2)))) (Name (convertToBS str))


checkType :: Expr -> Operand
checkType (Var var) = LocalReference double (Name (convertToBS var))
checkType (Lit (Floating lit)) = ConstantOperand (C.Float $ F.Double (lit))
checkType _ = ConstantOperand (C.Float $ F.Double (0.0))

checkTypeTab :: [Expr] -> [Operand]
checkTypeTab ((Var var):exps) = ([LocalReference double (Name (convertToBS var))] ++ checkTypeTab exps)
checkTypeTab ((Lit (Floating lit)):exps) = ([ConstantOperand (C.Float $ F.Double (lit))] ++ checkTypeTab exps)
checkTypeTab _ = ([ConstantOperand (C.Float $ F.Double (0.0))])


returnFun :: Operator -> Operand -> Operand -> Instruction
returnFun Plus op1 op2 = fadd op1 op2
returnFun Minus op1 op2 = fsub op1 op2
returnFun Mult op1 op2 = fmul op1 op2
returnFun Div op1 op2 = fdiv op1 op2
