{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- Chunk
-}

module Chunk (
    OpCode(..),
    initChunk,
    writeChunk,
    addConstant,
    toByte,
    fromByte,
    varTypeToWord8
) where

import Data.Word (Word8)
import Types.Base (Chunk(..), Variable(..), VarType(..))

data OpCode
    = OpConstant
    | OpNil
    | OpTrue
    | OpFalse
    | OpPop
    | OpGetLocal
    | OpSetLocal
    | OpGetGlobal
    | OpDefineGlobal
    | OpSetGlobal
    | OpGetUpvalue
    | OpSetUpvalue
    | OpGetProperty
    | OpSetProperty
    | OpGetSuper
    | OpEqual
    | OpGreater
    | OpLess
    | OpAdd
    | OpSubtract
    | OpMultiply
    | OpDivide
    | OpModulo
    | OpNot
    | OpNegate
    | OpPrint
    | OpJump
    | OpJumpIfFalse
    | OpLoop
    | OpCall
    | OpInvoke
    | OpSuperInvoke
    | OpClosure
    | OpCloseUpvalue
    | OpReturn
    | OpClass
    | OpInherit
    | OpMethod
    | OpGetAtIndex
    | OpSetAtIndex
    | OpPrintLn
    | OpSetRecursionLimit
    deriving (Show, Eq, Enum, Bounded)

initChunk :: Chunk
initChunk = Chunk
    { chunkCode = []
    , chunkLines = []
    , chunkConstants = []
    }

writeChunk :: Chunk -> Word8 -> Int -> Chunk
writeChunk chunk byte line = chunk
    { chunkCode = chunkCode chunk ++ [byte]
    , chunkLines = chunkLines chunk ++ [line]
    }

addConstant :: Chunk -> Variable -> (Chunk, Int)
addConstant chunk value =
    let newConstants = chunkConstants chunk ++ [value]
        newChunk = chunk { chunkConstants = newConstants }
    in (newChunk, length newConstants - 1)

toByte :: OpCode -> Word8
toByte = fromIntegral . fromEnum

fromByte :: Word8 -> Maybe OpCode
fromByte byte =
    if byte < fromIntegral (fromEnum (maxBound :: OpCode))
        then Just $ toEnum $ fromIntegral byte
        else Nothing

varTypeToWord8 :: VarType -> Word8
varTypeToWord8 VarInteger = 0
varTypeToWord8 VarBool = 1
varTypeToWord8 VarString = 2
varTypeToWord8 VarDouble = 3
varTypeToWord8 VarAny = 4
varTypeToWord8 VarClass = 5
varTypeToWord8 (VarTuple _) = 6
varTypeToWord8 VarMap {} = 7
