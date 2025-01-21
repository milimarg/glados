{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- Debug
-}

module Debug.Debug (
    disassembleChunk,
    disassembleInstruction,
    generateLineText,
    generateInstrText,
    interpretOpCode,
    lookupConstant,
    handleInvalidConstant,
    closureInstruction,
    invokeInstruction,
    listActionInstruction,
    typeConstantInstruction
) where

import qualified Data.Text as T
import Data.Word (Word8, Word16)
import Types.Base (Chunk(..))
import Chunk (OpCode(..))
import Text.Printf (printf)
import Utils (safeIndex)
import Data.Bits (Bits(..))

disassembleChunk :: Chunk -> T.Text -> T.Text
disassembleChunk chunk name =
    T.concat [T.pack $ printf "== %s ==\n"
    (T.unpack name), disassembleInstructions chunk 0]

disassembleInstructions :: Chunk -> Int -> T.Text
disassembleInstructions chunk offset
    | offset >= length (chunkCode chunk) = T.empty
    | otherwise =
        let (newText, newOffset) = disassembleInstruction chunk offset
        in T.concat [newText, disassembleInstructions chunk newOffset]

disassembleInstruction :: Chunk -> Int -> (T.Text, Int)
disassembleInstruction chunk offset =
    let lineText = generateLineText chunk offset
        instrText = generateInstrText chunk offset
    in (T.concat [lineText, fst instrText], snd instrText)

generateLineText :: Chunk -> Int -> T.Text
generateLineText chunk offset =
    case (safeIndex (chunkLines chunk) offset,
        safeIndex (chunkLines chunk) (offset - 1)) of
        (Just currentLine, Just prevLine)
            | currentLine == prevLine -> T.pack "   | "
        (Just currentLine, _) -> T.pack $ printf "%4d " currentLine
        _ -> T.pack "   ? "

generateInstrText :: Chunk -> Int -> (T.Text, Int)
generateInstrText chunk offset =
    case safeIndex (chunkCode chunk) offset of
        Just instr -> case fromByte instr of
            Just op -> interpretOpCode op chunk offset
            Nothing ->
                (T.pack $ printf "Unknown opcode %d\n" instr, offset + 1)
        Nothing -> (T.pack "Invalid offset\n", offset + 1)

interpretOpCode :: OpCode -> Chunk -> Int -> (T.Text, Int)
interpretOpCode OpConstant ch       = constantInstruction "OP_CONSTANT" ch
interpretOpCode OpNil _             = simpleInstruction   "OP_NIL"
interpretOpCode OpTrue _            = simpleInstruction   "OP_TRUE"
interpretOpCode OpFalse _           = simpleInstruction   "OP_FALSE"
interpretOpCode OpPop _             = simpleInstruction   "OP_POP"
interpretOpCode OpGetLocal ch       = byteInstruction     "OP_GET_LOCAL" ch
interpretOpCode OpSetLocal ch       = byteInstruction     "OP_SET_LOCAL" ch
interpretOpCode OpGetGlobal ch      = constantInstruction "OP_GET_GLOBAL" ch
interpretOpCode OpDefineGlobal ch   =
    typeConstantInstruction "OP_DEFINE_GLOBAL" ch
interpretOpCode OpSetGlobal ch      = constantInstruction "OP_SET_GLOBAL" ch
interpretOpCode OpGetUpvalue ch     = byteInstruction     "OP_GET_UPVALUE" ch
interpretOpCode OpSetUpvalue ch     = byteInstruction     "OP_SET_UPVALUE" ch
interpretOpCode OpGetProperty ch    = constantInstruction "OP_GET_PROPERTY" ch
interpretOpCode OpSetProperty ch    = constantInstruction "OP_SET_PROPERTY" ch
interpretOpCode OpGetSuper ch       = constantInstruction "OP_GET_SUPER" ch
interpretOpCode OpEqual _           = simpleInstruction   "OP_EQUAL"
interpretOpCode OpGreater _         = simpleInstruction   "OP_GREATER"
interpretOpCode OpLess _            = simpleInstruction   "OP_LESS"
interpretOpCode OpAdd _             = simpleInstruction   "OP_ADD"
interpretOpCode OpSubtract _        = simpleInstruction   "OP_SUBTRACT"
interpretOpCode OpMultiply _        = simpleInstruction   "OP_MULTIPLY"
interpretOpCode OpDivide _          = simpleInstruction   "OP_DIVIDE"
interpretOpCode OpModulo _          = simpleInstruction   "OP_MODULO"
interpretOpCode OpNot _             = simpleInstruction   "OP_NOT"
interpretOpCode OpNegate _          = simpleInstruction   "OP_NEGATE"
interpretOpCode OpPrint _           = simpleInstruction   "OP_PRINT"
interpretOpCode OpPrintLn _         = simpleInstruction   "OP_PRINTLN"
interpretOpCode OpSetRecursionLimit _ =
    simpleInstruction "OP_SETRECURSIONLIMIT"
interpretOpCode OpJump ch           = jumpInstruction     "OP_JUMP" 1 ch
interpretOpCode OpJumpIfFalse ch  = jumpInstruction     "OP_JUMP_IF_FALSE" 1 ch
interpretOpCode OpLoop ch           = jumpInstruction     "OP_LOOP" (-1) ch
interpretOpCode OpCall ch           = byteInstruction     "OP_CALL" ch
interpretOpCode OpInvoke ch         = invokeInstruction   "OP_INVOKE" ch
interpretOpCode OpSuperInvoke ch    = invokeInstruction   "OP_SUPER_INVOKE" ch
interpretOpCode OpClosure ch        = closureInstruction  ch
interpretOpCode OpCloseUpvalue _    = simpleInstruction   "OP_CLOSE_UPVALUE"
interpretOpCode OpReturn _          = simpleInstruction   "OP_RETURN"
interpretOpCode OpClass ch          = constantInstruction "OP_CLASS" ch
interpretOpCode OpInherit _         = simpleInstruction   "OP_INHERIT"
interpretOpCode OpMethod ch         = constantInstruction "OP_METHOD" ch
interpretOpCode OpGetAtIndex ch = listActionInstruction "OP_GET_AT_INDEX" ch
interpretOpCode OpSetAtIndex ch = listActionInstruction "OP_SET_AT_INDEX" ch

typeConstantInstruction :: String -> Chunk -> Int -> (T.Text, Int)
typeConstantInstruction name chunk offset =
    case safeIndex (chunkCode chunk) (offset + 1) of
        Just constant ->
            case safeIndex (chunkConstants chunk) (fromIntegral constant) of
                Just value -> (T.pack $ printf "%04d    | %-16s %4d '%s'\n"
                            offset name constant (show value), offset + 3)
                Nothing -> (T.pack "Invalid index\n", offset + 3)
        Nothing -> (T.pack "Invalid offset for constant\n", offset + 3)


constantInstruction :: String -> Chunk -> Int -> (T.Text, Int)
constantInstruction name chunk offset =
    case safeIndex (chunkCode chunk) (offset + 1) of
        Just constant ->
            case safeIndex (chunkConstants chunk) (fromIntegral constant) of
                Just value -> (T.pack $ printf "%04d    | %-16s %4d '%s'\n"
                            offset name constant (show value), offset + 2)
                Nothing -> (T.pack "Invalid index\n", offset + 2)
        Nothing -> (T.pack "Invalid offset for constant\n", offset + 2)

listActionInstruction :: String -> Chunk -> Int -> (T.Text, Int)
listActionInstruction name chunk offset =
    case safeIndex (chunkCode chunk) (offset + 1) of
        Just byte1 -> case safeIndex (chunkCode chunk) (offset + 2) of
            Just byte2 ->
                let byte1Word16 = fromIntegral byte1 :: Word16
                    listIndex = (byte1Word16 `shiftL` 8) .|. fromIntegral byte2
                in (T.pack $ printf "%04d    | %-16s %4d for index\n"
                        offset name listIndex, offset + 3)
            Nothing -> (T.pack "Invalid offset for second byte\n", offset + 3)
        Nothing -> (T.pack "Invalid offset for first byte\n", offset + 3)

invokeInstruction :: String -> Chunk -> Int -> (T.Text, Int)
invokeInstruction name chunk offset =
    case (safeIndex (chunkCode chunk) (offset + 1),
        safeIndex (chunkCode chunk) (offset + 2)) of
        (Just constant, Just argCount) ->
            case safeIndex (chunkConstants chunk) (fromIntegral constant) of
                Just value ->
                    (T.pack $ printf "%04d    | %-16s (%d args) %4d '%s'\n"
                    offset name argCount constant (show value), offset + 3)
                Nothing -> (T.pack "Invalid constant index\n", offset + 3)
        _ -> (T.pack "Invalid offset for invoke\n", offset + 3)

simpleInstruction :: String -> Int -> (T.Text, Int)
simpleInstruction name offset =
    (T.pack $ printf "%04d    | %s\n" offset name, offset + 1)

byteInstruction :: String -> Chunk -> Int -> (T.Text, Int)
byteInstruction name chunk offset =
    case safeIndex (chunkCode chunk) (offset + 1) of
        Just slot -> (T.pack $
            printf "%04d    | %-16s %4d\n" offset name slot, offset + 2)
        Nothing -> (T.pack "Invalid offset for byte\n", offset + 2)

jumpInstruction :: String -> Int -> Chunk -> Int -> (T.Text, Int)
jumpInstruction name sign chunk offset =
    case (safeIndex (chunkCode chunk) (offset + 1),
        safeIndex (chunkCode chunk) (offset + 2)) of
        (Just h, Just l) ->
            let jump = (fromIntegral h :: Int) * 256 + (fromIntegral l :: Int)
                target = offset + 3 + sign * jump
            in (T.pack $ printf "%04d    | %-16s %4d -> %d\n"
                offset name offset target, offset + 3)
        _ -> (T.pack "Invalid offset for jump\n", offset + 3)

lookupConstant :: Chunk -> Int -> Maybe (T.Text, Int)
lookupConstant chunk offset = do
    constant <- safeIndex (chunkCode chunk) (offset + 1)
    value <- safeIndex (chunkConstants chunk) (fromIntegral constant)
    return (T.pack $ printf "%04d    | %-16s %4d '%s'\n" offset "OP_CLOSURE"
            constant (show value), offset + 2)

handleInvalidConstant :: Int -> T.Text -> Int -> (T.Text, Int)
handleInvalidConstant offset opName constant =
    (T.pack $ printf "%04d    | %-16s %4d (invalid constant index)\n"
        offset opName constant, offset + 2)

closureInstruction :: Chunk -> Int -> (T.Text, Int)
closureInstruction chunk offset = case lookupConstant chunk offset of
    Just result -> result
    Nothing -> handleInvalidConstant offset (T.pack "OP_CLOSURE") (offset + 1)

fromByte :: Word8 -> Maybe OpCode
fromByte byte =
    if byte <= fromIntegral (fromEnum (maxBound :: OpCode))
    then Just $ toEnum $ fromIntegral byte
    else Nothing
