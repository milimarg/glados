{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- ChunkSpec
-}

{-# LANGUAGE OverloadedStrings #-}

module ChunkSpec (
    spec
) where

import Test.Hspec
import Chunk
import Types.Base (VarType(..), Variable(..), Chunk(..), Object(..), Value(..))

spec :: Spec
spec = do
    describe "OpCode" $ do
        it "should correctly convert to byte" $ do
            toByte OpConstant `shouldBe` 0
            toByte OpNil `shouldBe` 1
            toByte OpTrue `shouldBe` 2
            toByte OpFalse `shouldBe` 3
            toByte OpPop `shouldBe` 4

        it "should correctly convert from byte" $ do
            fromByte 0 `shouldBe` Just OpConstant
            fromByte 1 `shouldBe` Just OpNil
            fromByte 2 `shouldBe` Just OpTrue
            fromByte 3 `shouldBe` Just OpFalse
            fromByte 4 `shouldBe` Just OpPop
            fromByte 255 `shouldBe` Nothing

        describe "Show" $ do
            it "should correctly show OpConstant" $ do
                show OpConstant `shouldBe` "OpConstant"
            it "should correctly show OpNil" $ do
                show OpNil `shouldBe` "OpNil"
            it "should correctly show OpTrue" $ do
                show OpTrue `shouldBe` "OpTrue"
            it "should correctly show OpFalse" $ do
                show OpFalse `shouldBe` "OpFalse"
            it "should correctly show OpPop" $ do
                show OpPop `shouldBe` "OpPop"

        describe "Eq" $ do
            it "should be equal for the same OpCode" $ do
                OpConstant `shouldBe` OpConstant
                OpNil `shouldBe` OpNil
                OpTrue `shouldBe` OpTrue
                OpFalse `shouldBe` OpFalse
                OpPop `shouldBe` OpPop
            it "should not be equal for different OpCodes" $ do
                OpConstant `shouldNotBe` OpNil
                OpTrue `shouldNotBe` OpFalse
                OpPop `shouldNotBe` OpConstant

        describe "Enum" $ do
            it "should convert fromEnum to the correct integer" $ do
                fromEnum OpConstant `shouldBe` 0
                fromEnum OpNil `shouldBe` 1
                fromEnum OpTrue `shouldBe` 2
                fromEnum OpFalse `shouldBe` 3
                fromEnum OpPop `shouldBe` 4

            it "should convert toEnum to the correct OpCode" $ do
                toEnum 0 `shouldBe` OpConstant
                toEnum 1 `shouldBe` OpNil
                toEnum 2 `shouldBe` OpTrue
                toEnum 3 `shouldBe` OpFalse
                toEnum 4 `shouldBe` OpPop

    describe "initChunk" $ do
        it "should initialize a Chunk with empty code and constants" $ do
            let chunk = initChunk
            chunkCode chunk `shouldBe` []
            chunkConstants chunk `shouldBe` []

    describe "writeChunk" $ do
        it "should append a byte to the chunk code" $ do
            let chunk = initChunk
            let updatedChunk = writeChunk chunk 10 1
            chunkCode updatedChunk `shouldBe` [10]
            chunkLines updatedChunk `shouldBe` [1]

        it "should append multiple bytes" $ do
            let chunk = initChunk
            let updatedChunk1 = writeChunk chunk 10 1
            let updatedChunk2 = writeChunk updatedChunk1 20 2
            chunkCode updatedChunk2 `shouldBe` [10, 20]
            chunkLines updatedChunk2 `shouldBe` [1, 2]

    describe "addConstant" $ do
        it "should add a constant to the chunk" $ do
            let chunk = initChunk
            let variable = Variable VarInteger (VNumber 5)
            let (updatedChunk, index) = addConstant chunk variable
            chunkConstants updatedChunk `shouldBe` [variable]
            index `shouldBe` 0

        it "should update the constant index when adding new constants" $ do
            let chunk = initChunk
            let variable1 = Variable VarInteger (VNumber 5)
            let (updatedChunk1, index1) = addConstant chunk variable1
            let variable2 = Variable VarString (VObj (OString "test"))
            let (updatedChunk2, index2) = addConstant updatedChunk1 variable2
            chunkConstants updatedChunk2 `shouldBe` [variable1, variable2]
            index1 `shouldBe` 0
            index2 `shouldBe` 1

    describe "varTypeToWord8" $ do
        it "should convert VarInteger to 0" $ do
            varTypeToWord8 VarInteger `shouldBe` 0

        it "should convert VarBool to 1" $ do
            varTypeToWord8 VarBool `shouldBe` 1

        it "should convert VarString to 2" $ do
            varTypeToWord8 VarString `shouldBe` 2

        it "should convert VarDouble to 3" $ do
            varTypeToWord8 VarDouble `shouldBe` 3

        it "should convert VarAny to 4" $ do
            varTypeToWord8 VarAny `shouldBe` 4

        it "should convert VarClass to 5" $ do
            varTypeToWord8 VarClass `shouldBe` 5

        it "should convert VarTuple to 6" $ do
            varTypeToWord8 (VarTuple [VarInteger, VarString, VarBool]) `shouldBe` 6
