{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- REPLSpec
-}

module InputHandling.REPLSpec (
    spec
) where

import Test.Hspec
import InputHandling.REPL
import qualified Data.Text as T
import VM ()

spec :: Spec
spec = do
    describe "parseCommand" $ do
        it "parses :quit command" $
            parseCommand ":quit" `shouldBe` Quit
        it "parses :q command" $
            parseCommand ":q" `shouldBe` Quit
        it "parses :debug on command" $
            parseCommand ":debug on" `shouldBe` DebugMode True
        it "parses :d on command" $
            parseCommand ":d on" `shouldBe` DebugMode True
        it "parses :debug off command" $
            parseCommand ":debug off" `shouldBe` DebugMode False
        it "parses :d off command" $
            parseCommand ":d off" `shouldBe` DebugMode False
        it "parses :help command" $
            parseCommand ":help" `shouldBe` Help "help"
        it "parses :h command" $
            parseCommand ":h" `shouldBe` Help "help"
        it "parses :clear command" $
            parseCommand ":clear" `shouldBe` Clear
        it "parses :c command" $
            parseCommand ":c" `shouldBe` Clear
        it "parses :load command" $
            parseCommand ":load file.txt" `shouldBe` LoadFile "file.txt"
        it "parses :l command" $
            parseCommand ":l file.txt" `shouldBe` LoadFile "file.txt"
        it "parses :dump-history command" $
            parseCommand ":dump-history" `shouldBe` DumpHistory
        it "parses :dh command" $
            parseCommand ":dh" `shouldBe` DumpHistory
        it "parses :dump-env command" $
            parseCommand ":dump-env" `shouldBe` DumpEnv
        it "parses :de command" $
            parseCommand ":de" `shouldBe` DumpEnv
        it "parses :reset command" $
            parseCommand ":reset" `shouldBe` Reset
        it "parses :r command" $
            parseCommand ":r" `shouldBe` Reset
        it "parses code evaluation" $ do
            parseCommand "print(1)" `shouldBe` EvalCode (T.pack "print(1)")

    describe "prepareCode" $ do
        it "adds semicolon if missing" $
            prepareCode (T.pack "print(1)") `shouldBe` T.pack "print(1);"
        it "does not add semicolon if already present" $
            prepareCode (T.pack "print(1);") `shouldBe` T.pack "print(1);"
        it "does not add semicolon if ends with }" $
            prepareCode (T.pack "if (true) { print(1) }") `shouldBe` T.pack "if (true) { print(1) }"

    describe "getHelp" $ do
        it "shows help for quit" $
            getHelp "quit" `shouldBe` showQuitHelp
        it "shows help for debug" $
            getHelp "debug" `shouldBe` showDebugHelp
        it "shows help for clear" $
            getHelp "clear" `shouldBe` showClearHelp
        it "shows help for load" $
            getHelp "load" `shouldBe` showLoadHelp
        it "shows help for dump-history" $
            getHelp "dump-history" `shouldBe` showDumpHistoryHelp
        it "shows help for dump-env" $
            getHelp "dump-env" `shouldBe` showDumpEnvHelp
        it "shows help for reset" $
            getHelp "reset" `shouldBe` showResetHelp
        it "shows general help for unknown command" $
            getHelp "unknown" `shouldBe` showGeneralHelp
