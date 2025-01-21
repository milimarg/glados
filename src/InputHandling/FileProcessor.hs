{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- FileProcessor
-}

module InputHandling.FileProcessor (
    runFileMode,
    runBatchMode,
    processStdinContent,
    runCompiler,
    readAndCombineFiles,
) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (isEOF)
import System.Exit (exitWith, ExitCode(..))
import VM (interpret, InterpretResult(..), VMEnv(..), initVM)
import InputHandling.Args (Flags(..))
import Control.Monad (when, unless)
import Control.Exception
import Data.Time.Clock (getCurrentTime)
import Compiler (compile)
import Serialize (dumpFunction, loadFunction)
import Types.Base (Function)
import Data.Functor (void)
import System.Console.ANSI (setSGR, Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..))

runFileMode :: [FilePath] -> Flags -> IO ()
runFileMode paths flags = catch (runFileModeFunction paths flags) handleExit

runFileModeFunction :: [FilePath] -> Flags -> IO ()
runFileModeFunction paths flags = do
    currentTime <- getCurrentTime
    let initialVMEnv = initVM (debugMode flags) currentTime
    let debug = debugMode flags
    case runPathFlag flags of
        Nothing -> handleNoInputFile paths flags initialVMEnv debug True
        Just inputFile -> handleInputFile inputFile initialVMEnv debug True

handleNoInputFile :: [FilePath] -> Flags -> VMEnv -> Bool -> Bool -> IO ()
handleNoInputFile paths flags initialVMEnv debug canThrow = do
    cmb <- readAndCombineFiles paths flags
    let opti = not $ noOptiFlag flags
    case outputPathFlag flags of
        Just outputFile -> compileToFile cmb outputFile debug opti canThrow
        Nothing -> void $ runCompiler cmb initialVMEnv debug opti canThrow

handleInputFile :: FilePath -> VMEnv -> Bool -> Bool -> IO ()
handleInputFile inputFile initialVMEnv debug canThrow =
    void $ loadAndRunFromFile inputFile initialVMEnv debug canThrow

readAndCombineFiles :: [FilePath] -> Flags -> IO T.Text
readAndCombineFiles paths flags =
    T.intercalate (T.pack "\n") <$> mapM (readFileWithDebug flags) paths

printLine :: String -> IO ()
printLine line
    | not (null line) && head line == '[' =
        setSGR [SetColor Foreground Vivid Red] >>
        putStrLn line >>
        setSGR [Reset]
    | otherwise = putStrLn line

printColoredLines :: String -> IO ()
printColoredLines = mapM_ printLine . lines

printError :: String -> IO ()
printError err =
    setSGR [SetColor Foreground Vivid Red] >>
    putStrLn err >>
    setSGR [Reset]

compileToFile :: T.Text -> FilePath -> Bool -> Bool -> Bool -> IO ()
compileToFile code filePath debug opti _canThrow =
    let (compileOutput, buff) = compile code debug opti
        joinedBuff = T.unpack $ T.intercalate (T.pack "\n") buff
    in case compileOutput of
        Nothing ->
            printError "Compilation error:" >>
            printColoredLines joinedBuff
        Just compiledCode ->
            printColoredLines joinedBuff >>
            dumpFunction filePath compiledCode

runCompiler :: T.Text -> VMEnv -> Bool -> Bool -> Bool -> IO VMEnv
runCompiler code env debug opti canThrow =
    let (compileOutput, buff) = compile code debug opti
        joinedBuff = T.unpack $ T.intercalate (T.pack "\n") buff
    in case compileOutput of
        Nothing ->
            printError "Compilation error:" >>
            printColoredLines joinedBuff >> return env
        Just compiledCode ->
            printColoredLines joinedBuff >> runVM compiledCode env canThrow

runVM :: Function -> VMEnv -> Bool -> IO VMEnv
runVM compiledCode env canThrow =
    let res@(_, newEnv) = interpret compiledCode env
    in handleInterpretResult res canThrow >> return newEnv

readFileWithDebug :: Flags -> FilePath -> IO T.Text
readFileWithDebug flags path =
    when (debugMode flags) (putStrLn $ "Reading file: " ++ path) >>
    TIO.readFile path

ensureNewlineAtEnd :: [T.Text] -> IO ()
ensureNewlineAtEnd buffer = unless
    (null buffer || T.null (last buffer) || T.last (last buffer) == '\n')
    $ putChar '\n'

handleInterpretResult :: (InterpretResult, VMEnv) -> Bool -> IO ()
handleInterpretResult (result, vm) canThrow =
    mapM_ (putStr . T.unpack) (envStdoutBuffer vm) >>
    ensureNewlineAtEnd (envStdoutBuffer vm) >>
    mapM_ (printError . T.unpack) (envStderrBuffer vm) >>
    case result of
        InterpretOk            -> return ()
        InterpretCompileError  -> printError "Compile error in files." >>
            when canThrow (throw (userError "Compile error in files."))
        InterpretRuntimeError  -> printError "Runtime error in files." >>
            when canThrow (throw (userError "Runtime error in files."))

loadAndRunFromFile :: FilePath -> VMEnv -> Bool -> Bool -> IO VMEnv
loadAndRunFromFile inputFile env debug canThrow = do
    when debug $ putStrLn $ "Running code from file: " ++ inputFile
    loadedCode <- loadFunction inputFile
    runVM loadedCode env canThrow

runBatchMode :: Flags -> IO ()
runBatchMode flags = catch (runBatchModeFunction flags) handleExit

runBatchModeFunction :: Flags -> IO ()
runBatchModeFunction flags = do
    currentTime <- getCurrentTime
    let initialVM = initVM (debugMode flags) currentTime
    processBatchInput flags initialVM

processBatchInput :: Flags -> VMEnv -> IO ()
processBatchInput flags env = do
    done <- isEOF
    unless done $ do
        line <- getLine
        handleBatchLine flags env line >>= processBatchInput flags

handleBatchLine :: Flags -> VMEnv -> String -> IO VMEnv
handleBatchLine flags env line =
    let code = T.pack line
    in runCompiler code env (debugMode flags) (not $ noOptiFlag flags) True

processStdinContent :: String -> Flags -> IO ()
processStdinContent content flags = do
    currentTime <- getCurrentTime
    let initialVMEnv = initVM (debugMode flags) currentTime
    let combinedCode = T.pack content
    void $ runCompiler combinedCode initialVMEnv
        (debugMode flags) (not $ noOptiFlag flags) True

handleExit :: SomeException -> IO ()
handleExit _ = exitWith (ExitFailure 84)
