{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- Main
-}

import System.IO (hIsTerminalDevice, stdin)
import InputHandling.Args (parseArgs, defaultFlags, Flags(..))
import InputHandling.REPL (runInteractiveMode)
import InputHandling.FileProcessor (runFileMode, runBatchMode)
import System.Environment (getArgs)
import Control.Exception (catch, SomeException)
import System.Exit (exitWith, ExitCode(..))

addStandardLibrary :: Flags -> [String] -> [String]
addStandardLibrary flags list
    | stdFlag flags = "./stdlib/stdmath.uf" : list
    | otherwise = list

main :: IO ()
main = do
    isInteractive <- hIsTerminalDevice stdin
    args <- getArgs
    (flags, remainingArgs) <- parseArgs defaultFlags args
    let finalRemainingArgs = addStandardLibrary flags remainingArgs
    runProgram finalRemainingArgs isInteractive flags `catch` handler

runProgram :: [String] -> Bool -> Flags -> IO ()
runProgram args isInteractive flags
    | Just path <- runPathFlag flags = runFileMode [path] flags
    | not (null args) = runFileMode args flags
    | isInteractive = runInteractiveMode flags
    | otherwise = runBatchMode flags

handler :: SomeException -> IO ()
handler _ = exitWith (ExitFailure 84)
