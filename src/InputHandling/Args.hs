{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- Args
-}

module InputHandling.Args
    ( Flags(..)
    , defaultFlags
    , parseArgs
    ) where

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Data.List (isPrefixOf)

data Flags = Flags
    { debugMode :: Bool
    , outputPathFlag :: Maybe FilePath
    , runPathFlag :: Maybe FilePath
    , stdFlag :: Bool
    , noOptiFlag :: Bool
    } deriving (Show, Eq)

defaultFlags :: Flags
defaultFlags = Flags
    { debugMode = False
    , outputPathFlag = Nothing
    , runPathFlag = Nothing
    , stdFlag = False
    , noOptiFlag = False
    }

parseArgs :: Flags -> [String] -> IO (Flags, [String])
parseArgs flags [] = return (flags, [])
parseArgs flags ("--std":rest) = parseArgs (flags { stdFlag = True }) rest
parseArgs flags ("-d":rest) = parseArgs (flags { debugMode = True }) rest
parseArgs flags ("--debug":rest) = parseArgs (flags { debugMode = True }) rest
parseArgs flags ("-O0":rest) = parseArgs (flags { noOptiFlag = True }) rest
parseArgs flags ("-r":rest) =  case rest of
    (path:rest') -> parseArgs (flags { runPathFlag = Just path }) rest'
    [] -> hPutStrLn stderr "Error: Missing argument for --run/-r flag"
        >> exitFailure
parseArgs flags ("--run":rest) = case rest of
    (path:rest') -> parseArgs (flags { runPathFlag = Just path }) rest'
    [] -> hPutStrLn stderr "Error: Missing argument for --run/-r flag"
        >> exitFailure
parseArgs flags ("-o":rest) =  case rest of
    (path:rest') -> parseArgs (flags { outputPathFlag = Just path }) rest'
    [] -> hPutStrLn stderr "Error: Missing argument for --output/-o flag"
        >> exitFailure
parseArgs flags ("--output":rest) = case rest of
    (path:rest') -> parseArgs (flags { outputPathFlag = Just path }) rest'
    [] -> hPutStrLn stderr "Error: Missing argument for --output/-o flag"
        >> exitFailure
parseArgs _ (arg:_) | "-" `isPrefixOf` arg =
    hPutStrLn stderr ("Unknown flag: " ++ arg) >> exitFailure
parseArgs flags (arg:rest) = do
    (updatedFlags, remainingArgs) <- parseArgs flags rest
    return (updatedFlags, arg : remainingArgs)
