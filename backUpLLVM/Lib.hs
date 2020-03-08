{-# LANGUAGE OverloadedStrings #-}

module Lib where

import System.IO
import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module

import Control.Monad.Except
import Data.ByteString.Char8 as BS
import Data.ByteString.Short

int :: Type
int = IntegerType 32

defAdd :: ShortByteString -> Definition
defAdd str = GlobalDefinition functionDefaults
  { name = Name str
  , parameters =
      ( [ Parameter int (Name "a") []
        , Parameter int (Name "b") [] ]
      , False )
  , returnType = int
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (Name "entry")
        [ Name "result" :=
            Add False  -- no signed wrap
                False  -- no unsigned wrap
                (LocalReference int (Name "a"))
                (LocalReference int (Name "b"))
                []]
        (Do $ Ret (Just (LocalReference int (Name "result"))) [])


module_ :: ShortByteString -> AST.Module
module_ arg1 = defaultModule
  { moduleName = "basic"
  , moduleDefinitions = [defAdd arg1]
  }


toLLVM :: AST.Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
  BS.putStrLn llvm


main :: IO ()
main = do
    let lol = "on_a_reussi"
    toLLVM (module_ (toShort(BS.pack lol)))
