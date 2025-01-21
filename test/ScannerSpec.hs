{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- ScannerSpec
-}

{-# LANGUAGE OverloadedStrings #-}

module ScannerSpec (
    spec
) where

import qualified Data.Text as T
import Test.Hspec
import Scanner
import Control.Monad.State (State, evalState)

spec :: Spec
spec = do
    describe "Scanner" $ do
        it "scans single character tokens" $ do
            let tokens = scanAllTokens "(){}[],.-+;/*%"
            map tokenType tokens `shouldBe`
                [ TokenLeftParen, TokenRightParen, TokenLeftBrace, TokenRightBrace
                , TokenLeftSqBrace, TokenRightSqBrace, TokenComma, TokenDot
                , TokenMinus, TokenPlus, TokenSemicolon, TokenEOF ]

        it "scans multi-character tokens" $ do
            let tokens = scanAllTokens "!= == <= >= ->"
            map tokenType tokens `shouldBe`
                [ TokenBangEqual, TokenEqualEqual, TokenLessEqual, TokenGreaterEqual
                , TokenMinus, TokenGreater, TokenEOF ]

        it "scans identifiers and keywords" $ do
            let tokens = scanAllTokens "and class false true nil if else println print return setRecursionLimit fun super this list tuple var for while"
            map tokenType tokens `shouldBe`
                [ TokenAnd, TokenClass, TokenFalse, TokenTrue, TokenNil, TokenIf
                , TokenElse, TokenPrintLn, TokenPrint, TokenReturn, TokenSetRecursionLimit
                , TokenFun, TokenSuper, TokenThis, TokenList, TokenTuple, TokenVar
                , TokenFor, TokenWhile, TokenEOF ]

        it "scans strings" $ do
            let tokens = scanAllTokens "\"hello\""
            map tokenType tokens `shouldBe` [TokenString, TokenEOF]
            map tokenLexeme tokens `shouldBe` ["hello", ""]

        it "scans numbers" $ do
            let tokens = scanAllTokens "123 45.67"
            map tokenType tokens `shouldBe` [TokenNumber, TokenNumber, TokenEOF]
            map tokenLexeme tokens `shouldBe` ["123", "45.67", ""]

        it "handles whitespace and comments" $ do
            let tokens = scanAllTokens " \t\n// comment\n/* multi\nline\ncomment */123"
            map tokenType tokens `shouldBe` [TokenNumber, TokenEOF]
            map tokenLexeme tokens `shouldBe` ["123", ""]

        it "handles unexpected characters" $ do
            let tokens = scanAllTokens "@"
            map tokenType tokens `shouldBe` [TokenError, TokenEOF]
            map tokenLexeme tokens `shouldBe` ["Unexpected character.", ""]

scanAllTokens :: T.Text -> [Token]
scanAllTokens input = evalState (collectTokens []) (initScanner input)

collectTokens :: [Token] -> State Scanner [Token]
collectTokens tokens = do
    token <- scanToken
    if tokenType token == TokenEOF
        then return (tokens ++ [token])
        else collectTokens (tokens ++ [token])
