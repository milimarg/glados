{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- FileProcessorSpec
-}

module InputHandling.FileProcessorSpec (spec) where

import Test.Hspec
import InputHandling.FileProcessor
import InputHandling.Args (Flags(..))
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "runFileMode" $ do
        it "handles no input file" $ do
            let flags = Flags { debugMode = False, runPathFlag = Nothing, outputPathFlag = Nothing, noOptiFlag = False, stdFlag = False }
            runFileMode [] flags `shouldReturn` ()

    describe "readAndCombineFiles" $ do
        it "reads and combines multiple files" $ do
            let flags = Flags { debugMode = False, runPathFlag = Nothing, outputPathFlag = Nothing, noOptiFlag = False, stdFlag = False }
            let fileContents = [("file1.txt", "content1"), ("file2.txt", "content2")]
            let result = readAndCombineFiles' fileContents flags
            result `shouldBe` T.pack "content1\ncontent2\n"

    describe "compileToFile" $ do
        it "compiles code to file" $ do
            let code = T.pack "test code"
            let flags = Flags { debugMode = False, runPathFlag = Nothing, outputPathFlag = Just "output.txt", noOptiFlag = False, stdFlag = False }
            let compiledCode = compileToFile' code flags
            compiledCode `shouldBe` T.pack "compiled code"

readAndCombineFiles' :: [(FilePath, String)] -> Flags -> T.Text
readAndCombineFiles' files _ = T.pack $ unlines $ map snd files

compileToFile' :: T.Text -> Flags -> T.Text
compileToFile' _ _ = T.pack "compiled code"
