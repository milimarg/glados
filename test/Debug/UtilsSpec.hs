{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- UtilsSpec
-}

module Debug.UtilsSpec (spec) where

import Test.Hspec
import Debug.Utils
import InputHandling.Args (Flags(..))

spec :: Spec
spec = do
    describe "debugLog" $ do
        it "logs the message and value when debugMode is True" $ do
            let flags = Flags { debugMode = True, outputPathFlag = Nothing, runPathFlag = Nothing, stdFlag = False, noOptiFlag = False }
            debugLog flags "Test message" (42 :: Integer) `shouldReturn` ()

        it "does not log anything when debugMode is False" $ do
            let flags = Flags { debugMode = False, outputPathFlag = Nothing, runPathFlag = Nothing, stdFlag = False, noOptiFlag = False }
            debugLog flags "Test message" (42 :: Integer) `shouldReturn` ()

    describe "traceValue" $ do
        it "traces the value with the given label" $ do
            let result = traceValue "Test label" (42 :: Integer)
            result `shouldBe` 42

    describe "traceFunction" $ do
        it "traces the input and output of the function" $ do
            let f x = x + 1
            let tracedF = traceFunction "Test function" f
            tracedF (41 :: Integer) `shouldBe` (42 :: Integer)

    describe "tracePoint" $ do
        it "traces the message and returns the value" $ do
            let result = tracePoint "Reached here" (42 :: Integer)
            result `shouldBe` 42

    describe "traceAndReturn" $ do
        it "traces the value and returns it in a monad" $ do
            result <- traceAndReturn "Test label" (42 :: Integer)
            result `shouldBe` 42

    describe "traceList" $ do
        it "traces each element in the list with the given label" $ do
            let result = traceList "Test label" ([1, 2, 3] :: [Integer])
            result `shouldBe` [1, 2, 3]

    describe "traceMap" $ do
        it "traces the input and output of the function for each element in the list" $ do
            let f x = x + 1
            let result = traceMap "Test function" f ([1, 2, 3] :: [Integer])
            result `shouldBe` [2, 3, 4]

    describe "traceWhen" $ do
        it "traces the value when the condition is met" $ do
            let result = traceWhen (> 2) "Test label" (3 :: Integer)
            result `shouldBe` 3

        it "does not trace the value when the condition is not met" $ do
            let result = traceWhen (> 2) "Test label" (1 :: Integer)
            result `shouldBe` 1

    describe "buildTraceMessage" $ do
        it "builds a trace message for a successful result" $ do
            let result = buildTraceMessage "Test label" "input" (Right (42 :: Integer, "remaining"))
            result `shouldBe` "Test label - Input: \"input\", Result: 42, Remaining: \"remaining\""

        it "builds a trace message for an error result" $ do
            let result = buildTraceMessage "Test label" "input" (Left ("error" :: String) :: Either String (Int, String))
            result `shouldBe` "Test label - Input: \"input\", Error: error"
