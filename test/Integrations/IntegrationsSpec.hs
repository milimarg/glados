{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- IntegrationsSpec
-}

{-# LANGUAGE OverloadedStrings #-}

module Integrations.IntegrationsSpec (
    spec
) where

import Test.Hspec
import System.IO (hIsTerminalDevice, stdin)
import System.IO.Silently (capture_)
import InputHandling.FileProcessor
    ( runFileMode, runBatchMode )
import InputHandling.REPL (runInteractiveMode)
import InputHandling.Args (parseArgs, defaultFlags, Flags(..))
import Control.Exception (catch, SomeException)
import System.Exit (exitWith, ExitCode(..))

addStandardLibrary :: Flags -> [String] -> [String]
addStandardLibrary flags list
    | stdFlag flags = "./stdlib/stdmath.uf" : list
    | otherwise = list

spec :: Spec
spec = runSpecIntegrations

runSpecIntegrations :: Spec
runSpecIntegrations = specIntegrations
    [ "class-inheritance"
    , "class"
    , "closure"
    , "debug"
    , "error"
    , "factorial"
    , "fibonacci"
    , "for-advanced"
    , "for"
    , "func"
    , "inheritance"
    , "keywords"
    , "list"
    , "loop"
    , "macro"
    , "map"
    , "multiline-comment"
    , "optimization"
    , "recursion"
    , "return-self"
    , "return-types"
    , "scope-simple"
    , "scope"
    , "ternary-operator"
    , "testing"
    , "test"
    , "tuple"
    , "type"
    , "while"
  ]

specIntegrations :: [String] -> Spec
specIntegrations [] = do
    describe "end of integrationTests" $ do
        it "end" $ do
            ("end" :: String) `shouldBe` ("end" :: String)
specIntegrations (filepath:fs) = do
    describe ("integrationTests (" ++ filepath ++ ")") $ do
        it filepath $ do
            output <- capture_ (testFromMain ["./scripts/" ++ filepath ++ ".uf"])
            fileContent <- readFile ("./test/scripts/" ++ filepath ++ ".uf.txt")
            output `shouldBe` fileContent
    specIntegrations fs

testFromMain :: [String] -> IO ()
testFromMain args = do
    isInteractive <- hIsTerminalDevice stdin
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
