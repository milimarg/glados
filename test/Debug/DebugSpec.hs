module Debug.DebugSpec (spec) where

import Test.Hspec
import Debug.Debug
import Types.Base (Chunk(..), Variable(..), VarType(..), VarType(..), Value(..), Object(..))
import Chunk (OpCode(..))
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "disassembleChunk" $ do
        it "disassembles an empty chunk" $ do
            let chunk = Chunk [] [] []
            disassembleChunk chunk (T.pack "test") `shouldBe` T.pack "== test ==\n"
            disassembleChunk chunk (T.pack "test") `shouldBe` T.pack "== test ==\n"

        it "disassembles a chunk with one instruction" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpReturn] [0] []
            disassembleChunk chunk (T.pack "test") `shouldBe` T.pack "== test ==\n   0 0000    | OP_RETURN\n"
            disassembleChunk chunk (T.pack "test") `shouldBe` T.pack "== test ==\n   0 0000    | OP_RETURN\n"

    describe "disassembleInstruction" $ do
        it "disassembles a return instruction" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpReturn] [0] []
            disassembleInstruction chunk 0 `shouldBe` (T.pack "   0 0000    | OP_RETURN\n", 1)

        it "disassembles a constant instruction" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpConstant] [0] [Variable VarString (VObj (OString (T.pack "value")))]
            disassembleInstruction chunk 0 `shouldBe` (T.pack "   0 Invalid offset for constant\n", 2)

    describe "generateLineText" $ do
        it "generates line text for the first line" $ do
            let chunk = Chunk [] [1] []
            generateLineText chunk 0 `shouldBe` T.pack "   1 "

        it "generates line text for the same line" $ do
            let chunk = Chunk [] [1, 1] []
            generateLineText chunk 1 `shouldBe` T.pack "   | "

    describe "generateInstrText" $ do
        it "generates instruction text for an unknown opcode" $ do
            let chunk = Chunk [255] [0] []
            generateInstrText chunk 0 `shouldBe` (T.pack "Unknown opcode 255\n", 1)

        it "generates instruction text for a known opcode" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpReturn] [0] []
            generateInstrText chunk 0 `shouldBe` (T.pack "0000    | OP_RETURN\n", 1)

    describe "interpretOpCode" $ do
        it "interprets OP_RETURN" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpReturn] [0] []
            interpretOpCode OpReturn chunk 0 `shouldBe` (T.pack "0000    | OP_RETURN\n", 1)

        it "interprets OP_CONSTANT" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpConstant, 0] [0] [Variable VarString (VObj (OString (T.pack "value")))]
            interpretOpCode OpConstant chunk 0 `shouldBe` (T.pack "0000    | OP_CONSTANT         0 'value'\n", 2)

        it "interprets OP_GET_LOCAL" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpGetLocal, 1] [0] []
            interpretOpCode OpGetLocal chunk 0 `shouldBe` (T.pack "0000    | OP_GET_LOCAL        1\n", 2)

        it "interprets OP_JUMP" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpJump, 0, 2] [0] []
            interpretOpCode OpJump chunk 0 `shouldBe` (T.pack "0000    | OP_JUMP             0 -> 5\n", 3)

        it "interprets OP_CLOSURE" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpClosure, 0] [0] [Variable VarString (VObj (OString (T.pack "closure")))]
            interpretOpCode OpClosure chunk 0 `shouldBe` (T.pack "0000    | OP_CLOSURE          0 'closure'\n", 2)

    describe "lookupConstant" $ do
        it "looks up a valid constant" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpConstant, 0] [0] [Variable VarString (VObj (OString (T.pack "constant")))]
            lookupConstant chunk 0 `shouldBe` Just (T.pack "0000    | OP_CLOSURE          0 'constant'\n", 2)

        it "handles an invalid constant index" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpConstant, 1] [0] []
            lookupConstant chunk 0 `shouldBe` Nothing

    describe "listActionInstruction" $ do
        it "handles a valid list action instruction" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpGetAtIndex, 1, 2] [0] []
            listActionInstruction "OP_GET_AT_INDEX" chunk 0 `shouldBe` (T.pack "0000    | OP_GET_AT_INDEX   258 for index\n", 3)

        it "handles an invalid offset for first byte" $ do
            let chunk = Chunk [] [0] []
            listActionInstruction "OP_GET_AT_INDEX" chunk 0 `shouldBe` (T.pack "Invalid offset for first byte\n", 3)

        it "handles an invalid offset for second byte" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpGetAtIndex, 1] [0] []
            listActionInstruction "OP_GET_AT_INDEX" chunk 0 `shouldBe` (T.pack "Invalid offset for second byte\n", 3)

    describe "invokeInstruction" $ do
        it "handles a valid invoke instruction" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpInvoke, 0, 2] [0] [Variable VarString (VObj (OString (T.pack "invoke")))]
            invokeInstruction "OP_INVOKE" chunk 0 `shouldBe` (T.pack "0000    | OP_INVOKE        (2 args)    0 'invoke'\n", 3)

        it "handles an invalid constant index" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpInvoke, 1, 2] [0] []
            invokeInstruction "OP_INVOKE" chunk 0 `shouldBe` (T.pack "Invalid constant index\n", 3)

        it "handles an invalid offset for invoke" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpInvoke, 0] [0] []
            invokeInstruction "OP_INVOKE" chunk 0 `shouldBe` (T.pack "Invalid offset for invoke\n", 3)

    describe "handleInvalidConstant" $ do
        it "handles an invalid constant index" $ do
            handleInvalidConstant 0 (T.pack "OP_CLOSURE") 1 `shouldBe` (T.pack "0000    | OP_CLOSURE          1 (invalid constant index)\n", 2)

    describe "closureInstruction" $ do
        it "handles a valid closure instruction" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpClosure, 0] [0] [Variable VarString (VObj (OString (T.pack "closure")))]
            closureInstruction chunk 0 `shouldBe` (T.pack "0000    | OP_CLOSURE          0 'closure'\n", 2)

        it "handles an invalid closure instruction" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpClosure, 1] [0] []
            closureInstruction chunk 0 `shouldBe` (T.pack "0000    | OP_CLOSURE          1 (invalid constant index)\n", 2)

    describe "typeConstantInstruction" $ do
        it "handles a valid type constant instruction" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpDefineGlobal, 0] [0] [Variable VarString (VObj (OString (T.pack "typeConstant")))]
            typeConstantInstruction "OP_DEFINE_GLOBAL" chunk 0 `shouldBe` (T.pack "0000    | OP_DEFINE_GLOBAL    0 'typeConstant'\n", 3)

        it "handles an invalid constant index" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpDefineGlobal, 1] [0] []
            typeConstantInstruction "OP_DEFINE_GLOBAL" chunk 0 `shouldBe` (T.pack "Invalid index\n", 3)

        it "handles an invalid offset for constant" $ do
            let chunk = Chunk [] [0] []
            typeConstantInstruction "OP_DEFINE_GLOBAL" chunk 0 `shouldBe` (T.pack "Invalid offset for constant\n", 3)

    describe "interpretOpCode" $ do
        it "interprets OP_RETURN" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpReturn] [0] []
            interpretOpCode OpReturn chunk 0 `shouldBe` (T.pack "0000    | OP_RETURN\n", 1)

        it "interprets OP_CONSTANT" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpConstant, 0] [0] [Variable VarString (VObj (OString (T.pack "value")))]
            interpretOpCode OpConstant chunk 0 `shouldBe` (T.pack "0000    | OP_CONSTANT         0 'value'\n", 2)

        it "interprets OP_NIL" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpNil] [0] []
            interpretOpCode OpNil chunk 0 `shouldBe` (T.pack "0000    | OP_NIL\n", 1)

        it "interprets OP_TRUE" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpTrue] [0] []
            interpretOpCode OpTrue chunk 0 `shouldBe` (T.pack "0000    | OP_TRUE\n", 1)

        it "interprets OP_FALSE" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpFalse] [0] []
            interpretOpCode OpFalse chunk 0 `shouldBe` (T.pack "0000    | OP_FALSE\n", 1)

        it "interprets OP_POP" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpPop] [0] []
            interpretOpCode OpPop chunk 0 `shouldBe` (T.pack "0000    | OP_POP\n", 1)

        it "interprets OP_GET_LOCAL" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpGetLocal, 1] [0] []
            interpretOpCode OpGetLocal chunk 0 `shouldBe` (T.pack "0000    | OP_GET_LOCAL        1\n", 2)

        it "interprets OP_SET_LOCAL" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpSetLocal, 1] [0] []
            interpretOpCode OpSetLocal chunk 0 `shouldBe` (T.pack "0000    | OP_SET_LOCAL        1\n", 2)

        it "interprets OP_GET_GLOBAL" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpGetGlobal, 0] [0] [Variable VarString (VObj (OString (T.pack "global")))]
            interpretOpCode OpGetGlobal chunk 0 `shouldBe` (T.pack "0000    | OP_GET_GLOBAL       0 'global'\n", 2)

        it "interprets OP_DEFINE_GLOBAL" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpDefineGlobal, 0] [0] [Variable VarString (VObj (OString (T.pack "global")))]
            interpretOpCode OpDefineGlobal chunk 0 `shouldBe` (T.pack "0000    | OP_DEFINE_GLOBAL    0 'global'\n", 3)

        it "interprets OP_SET_GLOBAL" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpSetGlobal, 0] [0] [Variable VarString (VObj (OString (T.pack "global")))]
            interpretOpCode OpSetGlobal chunk 0 `shouldBe` (T.pack "0000    | OP_SET_GLOBAL       0 'global'\n", 2)

        it "interprets OP_GET_UPVALUE" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpGetUpvalue, 1] [0] []
            interpretOpCode OpGetUpvalue chunk 0 `shouldBe` (T.pack "0000    | OP_GET_UPVALUE      1\n", 2)

        it "interprets OP_SET_UPVALUE" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpSetUpvalue, 1] [0] []
            interpretOpCode OpSetUpvalue chunk 0 `shouldBe` (T.pack "0000    | OP_SET_UPVALUE      1\n", 2)

        it "interprets OP_GET_PROPERTY" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpGetProperty, 0] [0] [Variable VarString (VObj (OString (T.pack "property")))]
            interpretOpCode OpGetProperty chunk 0 `shouldBe` (T.pack "0000    | OP_GET_PROPERTY     0 'property'\n", 2)

        it "interprets OP_SET_PROPERTY" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpSetProperty, 0] [0] [Variable VarString (VObj (OString (T.pack "property")))]
            interpretOpCode OpSetProperty chunk 0 `shouldBe` (T.pack "0000    | OP_SET_PROPERTY     0 'property'\n", 2)

        it "interprets OP_GET_SUPER" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpGetSuper, 0] [0] [Variable VarString (VObj (OString (T.pack "super")))]
            interpretOpCode OpGetSuper chunk 0 `shouldBe` (T.pack "0000    | OP_GET_SUPER        0 'super'\n", 2)

        it "interprets OP_EQUAL" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpEqual] [0] []
            interpretOpCode OpEqual chunk 0 `shouldBe` (T.pack "0000    | OP_EQUAL\n", 1)

        it "interprets OP_GREATER" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpGreater] [0] []
            interpretOpCode OpGreater chunk 0 `shouldBe` (T.pack "0000    | OP_GREATER\n", 1)

        it "interprets OP_LESS" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpLess] [0] []
            interpretOpCode OpLess chunk 0 `shouldBe` (T.pack "0000    | OP_LESS\n", 1)

        it "interprets OP_ADD" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpAdd] [0] []
            interpretOpCode OpAdd chunk 0 `shouldBe` (T.pack "0000    | OP_ADD\n", 1)

        it "interprets OP_SUBTRACT" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpSubtract] [0] []
            interpretOpCode OpSubtract chunk 0 `shouldBe` (T.pack "0000    | OP_SUBTRACT\n", 1)

        it "interprets OP_MULTIPLY" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpMultiply] [0] []
            interpretOpCode OpMultiply chunk 0 `shouldBe` (T.pack "0000    | OP_MULTIPLY\n", 1)

        it "interprets OP_DIVIDE" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpDivide] [0] []
            interpretOpCode OpDivide chunk 0 `shouldBe` (T.pack "0000    | OP_DIVIDE\n", 1)

        it "interprets OP_MODULO" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpModulo] [0] []
            interpretOpCode OpModulo chunk 0 `shouldBe` (T.pack "0000    | OP_MODULO\n", 1)

        it "interprets OP_NOT" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpNot] [0] []
            interpretOpCode OpNot chunk 0 `shouldBe` (T.pack "0000    | OP_NOT\n", 1)

        it "interprets OP_NEGATE" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpNegate] [0] []
            interpretOpCode OpNegate chunk 0 `shouldBe` (T.pack "0000    | OP_NEGATE\n", 1)

        it "interprets OP_PRINT" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpPrint] [0] []
            interpretOpCode OpPrint chunk 0 `shouldBe` (T.pack "0000    | OP_PRINT\n", 1)

        it "interprets OP_PRINTLN" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpPrintLn] [0] []
            interpretOpCode OpPrintLn chunk 0 `shouldBe` (T.pack "0000    | OP_PRINTLN\n", 1)

        it "interprets OP_SETRECURSIONLIMIT" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpSetRecursionLimit] [0] []
            interpretOpCode OpSetRecursionLimit chunk 0 `shouldBe` (T.pack "0000    | OP_SETRECURSIONLIMIT\n", 1)

        it "interprets OP_JUMP" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpJump, 0, 2] [0] []
            interpretOpCode OpJump chunk 0 `shouldBe` (T.pack "0000    | OP_JUMP             0 -> 5\n", 3)

        it "interprets OP_JUMP_IF_FALSE" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpJumpIfFalse, 0, 2] [0] []
            interpretOpCode OpJumpIfFalse chunk 0 `shouldBe` (T.pack "0000    | OP_JUMP_IF_FALSE    0 -> 5\n", 3)

        it "interprets OP_LOOP" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpLoop, 0, 2] [0] []
            interpretOpCode OpLoop chunk 0 `shouldBe` (T.pack "0000    | OP_LOOP             0 -> 1\n", 3)

        it "interprets OP_CALL" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpCall, 1] [0] []
            interpretOpCode OpCall chunk 0 `shouldBe` (T.pack "0000    | OP_CALL             1\n", 2)

        it "interprets OP_INVOKE" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpInvoke, 0, 2] [0] [Variable VarString (VObj (OString (T.pack "invoke")))]
            interpretOpCode OpInvoke chunk 0 `shouldBe` (T.pack "0000    | OP_INVOKE        (2 args)    0 'invoke'\n", 3)

        it "interprets OP_SUPER_INVOKE" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpSuperInvoke, 0, 2] [0] [Variable VarString (VObj (OString (T.pack "superInvoke")))]
            interpretOpCode OpSuperInvoke chunk 0 `shouldBe` (T.pack "0000    | OP_SUPER_INVOKE  (2 args)    0 'superInvoke'\n", 3)

        it "interprets OP_CLOSURE" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpClosure, 0] [0] [Variable VarString (VObj (OString (T.pack "closure")))]
            interpretOpCode OpClosure chunk 0 `shouldBe` (T.pack "0000    | OP_CLOSURE          0 'closure'\n", 2)

        it "interprets OP_CLOSE_UPVALUE" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpCloseUpvalue] [0] []
            interpretOpCode OpCloseUpvalue chunk 0 `shouldBe` (T.pack "0000    | OP_CLOSE_UPVALUE\n", 1)

        it "interprets OP_CLASS" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpClass, 0] [0] [Variable VarString (VObj (OString (T.pack "class")))]
            interpretOpCode OpClass chunk 0 `shouldBe` (T.pack "0000    | OP_CLASS            0 'class'\n", 2)

        it "interprets OP_INHERIT" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpInherit] [0] []
            interpretOpCode OpInherit chunk 0 `shouldBe` (T.pack "0000    | OP_INHERIT\n", 1)

        it "interprets OP_METHOD" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpMethod, 0] [0] [Variable VarString (VObj (OString (T.pack "method")))]
            interpretOpCode OpMethod chunk 0 `shouldBe` (T.pack "0000    | OP_METHOD           0 'method'\n", 2)

        it "interprets OP_GET_AT_INDEX" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpGetAtIndex, 1, 2] [0] []
            interpretOpCode OpGetAtIndex chunk 0 `shouldBe` (T.pack "0000    | OP_GET_AT_INDEX   258 for index\n", 3)

        it "interprets OP_SET_AT_INDEX" $ do
            let chunk = Chunk [fromIntegral $ fromEnum OpSetAtIndex, 1, 2] [0] []
            interpretOpCode OpSetAtIndex chunk 0 `shouldBe` (T.pack "0000    | OP_SET_AT_INDEX   258 for index\n", 3)
