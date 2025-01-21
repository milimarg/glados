{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- REPL
-}

module InputHandling.REPL (
    runInteractiveMode,
    parseCommand,
    REPLCommand(..),
    matchCommand,
    replSettings,
    completionGenerator,
    getPrefixParts,
    finalizeSuggestions,
    createCompletion,
    prepareCode,
    getHelp,
    showQuitHelp,
    showClearHelp,
    showLoadHelp,
    showDumpHistoryHelp,
    showDumpEnvHelp,
    showResetHelp,
    showGeneralHelp,
    replLoop,
    showDebugHelp
) where

import System.Console.Haskeline
    ( getInputLine,
      outputStrLn,
      completeWordWithPrev,
      runInputT,
      Completion(Completion),
      InputT,
      Settings(..) )
import qualified Data.Text as T
import VM (VMEnv(..), initVM)
import InputHandling.Args (Flags(..))
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.List (isPrefixOf)
import Control.Monad.RWS (lift, MonadState (put, get), MonadIO (liftIO))
import Data.Time.Clock (getCurrentTime)
import Control.Monad (unless)
import Control.Exception.Base (IOException)
import Control.Exception (try)
import qualified Data.Map as Map
import InputHandling.FileProcessor (runCompiler)

data REPLCommand
    = Quit
    | DebugMode Bool
    | EvalCode T.Text
    | Help String
    | Clear
    | LoadFile FilePath
    | DumpHistory
    | DumpEnv
    | Reset
    deriving (Eq, Show)

parseCommand :: String -> REPLCommand
parseCommand input = matchCommand (words input)

matchCommand :: [String] -> REPLCommand
matchCommand [":quit"]         = Quit
matchCommand [":q"]            = Quit
matchCommand [":debug", "on"]  = DebugMode True
matchCommand [":d", "on"]      = DebugMode True
matchCommand [":debug", "off"] = DebugMode False
matchCommand [":d", "off"]     = DebugMode False
matchCommand [":help"]         = Help "help"
matchCommand [":h"]            = Help "help"
matchCommand [":clear"]        = Clear
matchCommand [":c"]            = Clear
matchCommand [":load", file]   = LoadFile file
matchCommand [":l", file]      = LoadFile file
matchCommand [":dump-history"] = DumpHistory
matchCommand [":dh"]           = DumpHistory
matchCommand [":dump-env"]     = DumpEnv
matchCommand [":de"]           = DumpEnv
matchCommand [":reset"]        = Reset
matchCommand [":r"]            = Reset
matchCommand [":help", cmd]    = Help cmd
matchCommand [":h", cmd]       = Help cmd
matchCommand other             = EvalCode $ T.pack (unwords other)

replSettings :: Settings (StateT VMEnv IO)
replSettings = Settings
    { complete = completeWordWithPrev Nothing "" completionGenerator
    , historyFile = Just "history.lhis"
    , autoAddHistory = True
    }

completionGenerator :: String -> String -> StateT VMEnv IO [Completion]
completionGenerator _ preceding = do
    prefixParts <- getPrefixParts preceding
    let prefix = last prefixParts
        context = init prefixParts
    vm <- get
    let globalKeys = map T.unpack (Map.keys (envGlobals vm))
        suggestions =
            filter (prefix `isPrefixOf`) (getAllCompletions ++ globalKeys)
    pure $ finalizeSuggestions context suggestions

getPrefixParts :: String -> StateT VMEnv IO [String]
getPrefixParts preceding =
    let tempPrefixParts = splitBy "( \t\r\n" preceding
    in pure $ if null tempPrefixParts then [""] else tempPrefixParts

finalizeSuggestions :: [String] -> [String] -> [Completion]
finalizeSuggestions context suggestions =
    case suggestions of
        [single] -> [createCompletion context single]
        _        -> map (createCompletion context) suggestions

createCompletion :: [String] -> String -> Completion
createCompletion context suggestion =
    Completion (concat (context ++ [suggestion])) suggestion True

splitBy :: String -> String -> [String]
splitBy delims str = go delims str ++
    ["" | not (null str) && last str `elem` delims]
  where
    go _ [] = []
    go ds (x:xs)
        | x `elem` ds = [x] : go ds xs
        | otherwise   = let (word, rest) = span (`notElem` ds) (x:xs)
                        in word : go ds rest

getAllCompletions :: [String]
getAllCompletions = keywords
    ++ builtinFunctions
    ++ builtinOperators
    ++ constants
    ++ replCommands

keywords :: [String]
keywords = [
    "if",
    "else",
    "list",
    "var",
    "and",
    "or"
    ]

builtinFunctions :: [String]
builtinFunctions = [
    "print",
    "printLn",
    "setRecursionLimit"
    ]

builtinOperators :: [String]
builtinOperators = [
    "+", "-", "*", "/", "%",
    ">", "<", ">=", "<=",
    "=", "!=", "!"
    ]

constants :: [String]
constants = [
    "true", "false", "nil"
    ]

replCommands :: [String]
replCommands = [
    ":quit", ":q",
    ":debug", ":d",
    ":help", ":h",
    ":clear", ":c",
    ":load", ":l",
    ":dump-history", ":dh",
    ":dump-env", ":de",
    ":reset", ":r"
    ]

cleanVMEnv :: VMEnv -> VMEnv -> VMEnv
cleanVMEnv newVM oldVM = (initVM (envDebugMode oldVM) (envStartTime oldVM))
    { envGlobals = envGlobals newVM
    }

runInteractiveMode :: Flags -> IO ()
runInteractiveMode flags = do
    putStrLn "Starting REPL. Type :quit to exit."
    currentTime <- getCurrentTime
    let initialVM = initVM (debugMode flags) currentTime
    evalStateT (runInputT replSettings $ replLoop flags) initialVM

processInput :: String -> InputT (StateT VMEnv IO) Bool
processInput input =
    let command = parseCommand input
    in handleCommand command >> return (input `elem` [":quit",":q"])

handleCommand :: REPLCommand -> InputT (StateT VMEnv IO) ()
handleCommand Quit             = handleExit
handleCommand (DebugMode dbg)  = toggleDebugMode dbg
handleCommand (Help cmd)       = showHelp cmd
handleCommand Clear            = clearTerminal
handleCommand (EvalCode code)  = evaluateCode code
handleCommand (LoadFile path)  = loadAndExecuteFile path
handleCommand DumpHistory      = dumpHistory
handleCommand DumpEnv          = dumpEnv
handleCommand Reset            = resetVM

loadAndExecuteFile :: FilePath -> InputT (StateT VMEnv IO) ()
loadAndExecuteFile path = do
    result <- liftIO $ safeReadFile path
    case result of
        Left err -> outputStrLn $ "Error loading file: " ++ err
        Right content ->
            outputStrLn ("Executing file: " ++ path) >>
            evaluateCode (T.pack content)

safeReadFile :: FilePath -> IO (Either String String)
safeReadFile path = do
    result <- try (readFile path) :: IO (Either IOException String)
    return $ case result of
        Left err   -> Left (show err)
        Right text -> Right text

handleExit :: InputT (StateT VMEnv IO) ()
handleExit = outputStrLn "Exiting REPL."

toggleDebugMode :: Bool -> InputT (StateT VMEnv IO) ()
toggleDebugMode dbg = do
    vm <- lift get
    lift $ put vm { envDebugMode = dbg }
    outputStrLn $ "Debug mode " ++ if dbg then "enabled" else "disabled"

showHelp :: String -> InputT (StateT VMEnv IO) ()
showHelp = outputStrLn . getHelp

getHelp :: String -> String
getHelp "quit"         = showQuitHelp
getHelp "q"            = showQuitHelp
getHelp "debug"        = showDebugHelp
getHelp "d"            = showDebugHelp
getHelp "clear"        = showClearHelp
getHelp "c"            = showClearHelp
getHelp "load"         = showLoadHelp
getHelp "l"            = showLoadHelp
getHelp "help"         = showGeneralHelp
getHelp "h"            = showGeneralHelp
getHelp "dump-history" = showDumpHistoryHelp
getHelp "dh"           = showDumpHistoryHelp
getHelp "dump-env"     = showDumpEnvHelp
getHelp "de"           = showDumpEnvHelp
getHelp "reset"        = showResetHelp
getHelp "r"            = showResetHelp
getHelp _              = showGeneralHelp

showGeneralHelp :: String
showGeneralHelp = unlines [
    "Available commands:",
    showQuitHelp, showDebugHelp,
    showClearHelp, showLoadHelp,
    showDumpHistoryHelp, showDumpEnvHelp,
    showResetHelp,
    ":help         (:h)     - Show this help message."
    ]

showQuitHelp :: String
showQuitHelp = ":quit         (:q)     - Exit the REPL."

showDebugHelp :: String
showDebugHelp =
    ":debug on     (:d on)  - Enable debug mode.\n" ++
    ":debug off    (:d off) - Disable debug mode."

showClearHelp :: String
showClearHelp = ":clear        (:c)     - Clear the terminal."

showLoadHelp :: String
showLoadHelp = ":load <file>  (:l)     - Load and execute a file."

showDumpHistoryHelp :: String
showDumpHistoryHelp = ":dump-history (:dh)    - Show the history of commands."

showDumpEnvHelp :: String
showDumpEnvHelp = ":dump-env     (:de)    - Dump the environment."

showResetHelp :: String
showResetHelp = ":reset        (:r)     - Reset the REPL state."

clearTerminal :: InputT (StateT VMEnv IO) ()
clearTerminal = liftIO $ putStr "\ESC[2J\ESC[H"

dumpHistory :: InputT (StateT VMEnv IO) ()
dumpHistory = do
    result <- liftIO $ safeReadFile "history.lhis"
    case result of
        Left err -> outputStrLn $ "Error loading history: " ++ err
        Right content -> outputStrLn content

dumpEnv :: InputT (StateT VMEnv IO) ()
dumpEnv = do
    vm <- lift get
    let globals = envGlobals vm
        debug = envDebugMode vm
    outputStrLn $ "Debug Mode: " ++ show debug
    outputStrLn "Global Variables:"
    mapM_ (outputStrLn . show) (Map.toList globals)
    outputStrLn $ show $ recursionLimit vm

resetVM :: InputT (StateT VMEnv IO) ()
resetVM = do
    outputStrLn "Resetting REPL state..."
    currentTime <- liftIO getCurrentTime
    let initialVM = initVM False currentTime
    lift $ put initialVM

evaluateCode :: T.Text -> InputT (StateT VMEnv IO) ()
evaluateCode code = do
    let preparedCode = prepareCode code
    vm <- lift get
    newVM <- liftIO $ runCompiler
        (T.unwords [preparedCode]) vm (envDebugMode vm) False False
    lift $ put (cleanVMEnv newVM vm)

prepareCode :: T.Text -> T.Text
prepareCode code = ensureEndsWithSemicolon $ T.stripEnd code

ensureEndsWithSemicolon :: T.Text -> T.Text
ensureEndsWithSemicolon trimmedCode
    | T.null trimmedCode = trimmedCode
    | lastChar /= ';' && lastChar /= '}' = T.snoc trimmedCode ';'
    | otherwise = trimmedCode
    where lastChar = T.last trimmedCode

replLoop :: Flags -> InputT (StateT VMEnv IO) ()
replLoop flags = do
    minput <- getInputLine "uf> "
    case minput of
        Nothing    -> handleExit
        Just input -> processInput input >>= (`unless` replLoop flags)
