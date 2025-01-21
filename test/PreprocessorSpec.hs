{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- PreprocessorSpec
-}

{-# LANGUAGE OverloadedStrings #-}

module PreprocessorSpec (spec) where

import Test.Hspec
import Preprocessor

spec :: Spec
spec = do
    describe "preprocess" $ do
        it "replaces default macros" $ do
            let input = "a ++\nb --\nc || d\nx && y\nprintuf z\nprintmiaou w"
            let expected = "a = a + 1\nb = b - 1\nc or d\nx and y\nprint \"uf \"; println  z\nprint \"miaou \"; println  w\n"
            preprocess input `shouldBe` expected

        it "handles #define macros" $ do
            let input = "#define foo bar\nfoo"
            let expected = "bar\n"
            preprocess input `shouldBe` expected

        it "handles nested macros" $ do
            let input = "#define foo bar\n#define bar baz\nfoo"
            let expected = "bar\n"
            preprocess input `shouldBe` expected

        it "handles macros inside strings" $ do
            let input = "print(\"a ++\")"
            let expected = "print(\"a ++\")\n"
            preprocess input `shouldBe` expected

        it "handles compound operators" $ do
            let input = "x += 1\ny -= 2\nz *= 3\nw /= 4\nv %= 5"
            let expected = "x = x + 1\ny = y - 2\nz = z * 3\nw = w / 4\nv = v % 5\n"
            preprocess input `shouldBe` expected

        it "handles infix functions" $ do
            let input = "a `foo` b"
            let expected = " foo(a, b)\n"
            preprocess input `shouldBe` expected

        it "handles escaped quotes" $ do
            let input = "print(\"\\\"a ++\\\"\")"
            let expected = "print(\"\\\"a ++\\\"\")\n"
            preprocess input `shouldBe` expected

        it "handles multiple lines with mixed content" $ do
            let input = "#define foo bar\nx += 1\nfoo `baz` y\nprint(\"a ++\")"
            let expected = "x = x + 1\n baz(bar, y)\nprint(\"a ++\")\n"
            preprocess input `shouldBe` expected
