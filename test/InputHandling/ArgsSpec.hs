{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- ArgsSpec
-}

module InputHandling.ArgsSpec (spec) where

import Test.Hspec
import InputHandling.Args
import System.Exit (ExitCode(..))

spec :: Spec
spec = do
    describe "parseArgs" $ do
        it "parses --std flag" $ do
            (flags, args) <- parseArgs defaultFlags ["--std"]
            flags `shouldBe` defaultFlags { stdFlag = True }
            args `shouldBe` []

        it "parses -d flag" $ do
            (flags, args) <- parseArgs defaultFlags ["-d"]
            flags `shouldBe` defaultFlags { debugMode = True }
            args `shouldBe` []

        it "parses --debug flag" $ do
            (flags, args) <- parseArgs defaultFlags ["--debug"]
            flags `shouldBe` defaultFlags { debugMode = True }
            args `shouldBe` []

        it "parses -O0 flag" $ do
            (flags, args) <- parseArgs defaultFlags ["-O0"]
            flags `shouldBe` defaultFlags { noOptiFlag = True }
            args `shouldBe` []

        it "parses -r flag with path" $ do
            (flags, args) <- parseArgs defaultFlags ["-r", "path/to/run"]
            flags `shouldBe` defaultFlags { runPathFlag = Just "path/to/run" }
            args `shouldBe` []

        it "parses --run flag with path" $ do
            (flags, args) <- parseArgs defaultFlags ["--run", "path/to/run"]
            flags `shouldBe` defaultFlags { runPathFlag = Just "path/to/run" }
            args `shouldBe` []

        it "parses -o flag with path" $ do
            (flags, args) <- parseArgs defaultFlags ["-o", "path/to/output"]
            flags `shouldBe` defaultFlags { outputPathFlag = Just "path/to/output" }
            args `shouldBe` []

        it "parses --output flag with path" $ do
            (flags, args) <- parseArgs defaultFlags ["--output", "path/to/output"]
            flags `shouldBe` defaultFlags { outputPathFlag = Just "path/to/output" }
            args `shouldBe` []

        it "returns unknown flag error" $ do
            parseArgs defaultFlags ["--unknown"] `shouldThrow` (== ExitFailure 1)

        it "returns missing argument error for -r flag" $ do
            parseArgs defaultFlags ["-r"] `shouldThrow` (== ExitFailure 1)

        it "returns missing argument error for --run flag" $ do
            parseArgs defaultFlags ["--run"] `shouldThrow` (== ExitFailure 1)

        it "returns missing argument error for -o flag" $ do
            parseArgs defaultFlags ["-o"] `shouldThrow` (== ExitFailure 1)

        it "returns missing argument error for --output flag" $ do
            parseArgs defaultFlags ["--output"] `shouldThrow` (== ExitFailure 1)

        it "parses non-flag arguments" $ do
            (flags, args) <- parseArgs defaultFlags ["arg1", "arg2"]
            flags `shouldBe` defaultFlags
            args `shouldBe` ["arg1", "arg2"]

        it "parses multiple flags together" $ do
            (flags, args) <- parseArgs defaultFlags ["--std", "-d", "-O0"]
            flags `shouldBe` defaultFlags { stdFlag = True, debugMode = True, noOptiFlag = True }
            args `shouldBe` []

        it "parses multiple flags with paths" $ do
            (flags, args) <- parseArgs defaultFlags ["-r", "path/to/run", "-o", "path/to/output"]
            flags `shouldBe` defaultFlags { runPathFlag = Just "path/to/run", outputPathFlag = Just "path/to/output" }
            args `shouldBe` []

        it "parses flags and non-flag arguments together" $ do
            (flags, args) <- parseArgs defaultFlags ["--std", "arg1", "-d", "arg2"]
            flags `shouldBe` defaultFlags { stdFlag = True, debugMode = True }
            args `shouldBe` ["arg1", "arg2"]

        it "parses flags with missing arguments and non-flag arguments" $ do
            parseArgs defaultFlags ["-r", "arg1", "--output"] `shouldThrow` (== ExitFailure 1)

        it "parses flags with invalid flag in between" $ do
            parseArgs defaultFlags ["--std", "--invalid", "-d"] `shouldThrow` (== ExitFailure 1)

        it "parses multiple flags with mixed order" $ do
            (flags, args) <- parseArgs defaultFlags ["-d", "--std", "-O0", "--output", "path/to/output", "-r", "path/to/run"]
            flags `shouldBe` defaultFlags { debugMode = True, stdFlag = True, noOptiFlag = True, outputPathFlag = Just "path/to/output", runPathFlag = Just "path/to/run" }
            args `shouldBe` []

        it "parses flags with non-flag arguments in between" $ do
            (flags, args) <- parseArgs defaultFlags ["--std", "arg1", "-d", "arg2", "-O0"]
            flags `shouldBe` defaultFlags { stdFlag = True, debugMode = True, noOptiFlag = True }
            args `shouldBe` ["arg1", "arg2"]

        it "parses flags with multiple non-flag arguments" $ do
            (flags, args) <- parseArgs defaultFlags ["--std", "arg1", "arg2", "-d", "arg3"]
            flags `shouldBe` defaultFlags { stdFlag = True, debugMode = True }
            args `shouldBe` ["arg1", "arg2", "arg3"]

        it "parses flags with empty arguments" $ do
            (flags, args) <- parseArgs defaultFlags []
            flags `shouldBe` defaultFlags
            args `shouldBe` []

        it "parses flags with only non-flag arguments" $ do
            (flags, args) <- parseArgs defaultFlags ["arg1", "arg2", "arg3"]
            flags `shouldBe` defaultFlags
            args `shouldBe` ["arg1", "arg2", "arg3"]
