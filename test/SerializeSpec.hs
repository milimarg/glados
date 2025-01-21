{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- SerializeSpec
-}

{-# LANGUAGE OverloadedStrings #-}

module SerializeSpec (spec) where

import Test.Hspec
import Serialize (dumpFunction, loadFunction)
import Types.Base (Function(..))
import Chunk (initChunk)
import System.Directory (removeFile)
import Control.Exception (bracket_)
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "Serialize" $ do
        it "correctly serializes and deserializes a Function" $ do
            let chunk = initChunk
            let func = Function
                    { functionArity = 2
                    , functionUpvalueCount = 0
                    , functionChunk = chunk
                    , functionName = Just (T.pack "example")
                    }
            let filePath = "testFunction.bin"
            bracket_ (dumpFunction filePath func) (removeFile filePath) (do
                    loadedFunc <- loadFunction filePath
                    loadedFunc `shouldBe` func)
