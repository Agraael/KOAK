module Linker where

import System.Process
import System.Directory
import System.FilePath
import System.Exit

llc = "llc-7"
ld = "ld"
crti = "/usr/lib/x86_64-linux-gnu/crti.o"
crtn = "/usr/lib/x86_64-linux-gnu/crtn.o"
crt1 = "/usr/lib/x86_64-linux-gnu/crt1.o"
ldLinux = "/lib64/ld-linux-x86-64.so.2"


shellCmd :: [String] -> String
shellCmd [] = ""
shellCmd [x] = x
shellCmd (x : xs) = x ++ " " ++ shellCmd xs

modifExtension :: String -> String -> String
modifExtension [] [] = ""
modifExtension path ext = dropExtension path ++ ext

koakCompile :: String -> String -> IO()
koakCompile llir out = do
    fileExist <- doesFileExist llir
    let obj = llir `modifExtension` ".o"
    if fileExist
        then system (shellCmd [llc, "-filetype=obj", llir, "-o", obj]) >>= \case
            (ExitFailure _) -> do
                putStrLn "Error : something went wrong with llc"
                exitWith $ ExitFailure 84
            ExitSuccess -> system (shellCmd [ld, obj, "-o", out, crti, crtn, crt1, "-dynamic-linker", ldLinux, "-lc"]) >>= \case
                (ExitFailure _) -> do
                    putStrLn "Error : something went wrong with ld"
                    exitWith $ ExitFailure 84
                ExitSuccess -> putStrLn "Compilation successfully done"
        else do
            putStrLn $ "Error : " ++ llir ++ " does not exist"
            exitWith $ ExitFailure 84
