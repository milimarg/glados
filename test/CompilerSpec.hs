{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- CompilerSpec
-}

module CompilerSpec (
    spec
) where

import Test.Hspec
import Data.Text (pack)
import qualified Data.Text as T
import Control.Monad.State ( gets, modify, runState )
import Types.Base (Chunk(..), Function(..), Variable(..), Value(..), VarType(..), Object(..))
import Compiler
import qualified Scanner as S
import qualified Chunk as C
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import VM (initVM, interpret, InterpretResult (..), VMEnv (..))
import Data.Time (UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (secondsToDiffTime)
import Control.Exception (evaluate)
import Data.Maybe (isJust, isNothing)

initTestState :: T.Text -> Bool -> Bool -> CompilerState
initTestState = initCompilerState

runCompilerTest :: CompilerM a -> CompilerState -> (Maybe a, CompilerState)
runCompilerTest action = runState (runMaybeT action)

spec :: Spec
spec = describe "Compiler" $ do
    describe "initCompiler" $ do
        it "should create a Compiler with TypeScript function type" $ do
            let compiler = initCompiler TypeScript Nothing
            compilerFunctionType compiler `shouldBe` TypeScript
            compilerLocalCount compiler `shouldBe` 0
            compilerScopeDepth compiler `shouldBe` 0

    describe "initCompilerState" $ do
        it "should initialize the compiler state with the correct settings" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            hadError state `shouldBe` False
            panicMode state `shouldBe` False
            debugMode state `shouldBe` True
            optiMode state `shouldBe` True
            scanner state `shouldBe` S.initScanner (pack source)

    describe "compile" $ do
        it "should compile successfully with valid source code" $ do
            let source = "var x = 5;"
            let (result, debug) = compile (pack source) True True
            result `shouldBe` Just (Function {functionArity = 0, functionUpvalueCount = 0, functionChunk = Chunk { chunkCode = [0, 1, 8, 0, 4, 1, 34], chunkLines = [1, 1, 1, 1, 1, 2, 2], chunkConstants = [Variable { varType = VarString, varValue = VObj (OString $ T.pack "x") }, Variable { varType = VarInteger, varValue = VNumber 5.0 }] }, functionName = Nothing })
            null debug `shouldBe` False

        it "should return Nothing and error buffer with invalid source" $ do
            let source = "invalid code"
            let (result, debug) = compile (pack source) True False
            result `shouldBe` Nothing
            debug `shouldNotBe` []

    describe "consume" $ do
        it "should advance when the token type matches" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenVar (T.pack "var") 1 1 }
            let (result, finalState) = runCompilerTest (consume S.TokenVar "Expected variable") initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenVar (T.pack "var") 1 1

        it "should throw an error when the token type does not match" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenIdentifier (T.pack "var") 1 1 }
            let (result, finalState) = runCompilerTest (consume S.TokenNumber "Expected number") initialState
            result `shouldBe` Just ()
            hadError finalState `shouldBe` True

    describe "consumeGet" $ do
        it "should consume the correct token and return the previous token" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenIdentifier (T.pack "var") 1 1 }
            let (result, finalState) = runCompilerTest (consumeGet S.TokenIdentifier "Expected variable") initialState
            result `shouldBe` Just (S.Token S.TokenIdentifier (T.pack "var") 1 1)
            currentToken finalState `shouldBe` S.Token S.TokenVar (T.pack "var") 1 1

    describe "check" $ do
        it "should return True when the token type matches" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenIdentifier (T.pack "var") 1 1 }
            let (result, _) = runCompilerTest (check S.TokenIdentifier) initialState
            result `shouldBe` Just True

        it "should return False when the token type does not match" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenIdentifier (T.pack "var") 1 1 }
            let (result, _) = runCompilerTest (check S.TokenNumber) initialState
            result `shouldBe` Just False

    describe "debugPrint" $ do
        it "should append the debug message to the debugBuffer" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { debugBuffer = [] }
            let (result, finalState) = runCompilerTest (debugPrint (T.pack "Debugging")) initialState
            result `shouldBe` Just ()
            debugBuffer finalState `shouldBe` [T.pack "Debugging"]

    describe "errorAt" $ do
        it "should set hadError and panicMode to True and add the error message to debugBuffer" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenIdentifier (T.pack "var") 1 1 }
            let (result, finalState) = runCompilerTest (errorAt (S.Token S.TokenIdentifier (T.pack "var") 1 1) (T.pack "Test error")) initialState
            result `shouldBe` Just ()
            hadError finalState `shouldBe` True
            panicMode finalState `shouldBe` True
            debugBuffer finalState `shouldBe` [T.pack "[line 1, col 1] Error at 'var': Test error"]

    describe "buildErrorMessage" $ do
        it "should create the correct error message" $ do
            let token = S.Token S.TokenIdentifier (T.pack "var") 1 1
            let message = T.pack "Test error"
            let errorMessage = buildErrorMessage token message
            errorMessage `shouldBe` T.pack "[line 1, col 1] Error at 'var': Test error"

    describe "formatTokenContext" $ do
        it "should format the context for a normal token" $ do
            let token = S.Token S.TokenIdentifier (T.pack "var") 1 1
            let ctxt = formatTokenContext token
            ctxt `shouldBe` T.pack " at 'var'"

        it "should handle EOF token correctly" $ do
            let token = S.Token S.TokenEOF T.empty 1 1
            let ctxt = formatTokenContext token
            ctxt `shouldBe` T.pack " at end"

        it "should handle TokenError token correctly" $ do
            let token = S.Token S.TokenError T.empty 1 1
            let ctxt = formatTokenContext token
            ctxt `shouldBe` T.empty

    describe "errorAtCurrent" $ do
        it "should call errorAt with the current token" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenIdentifier (T.pack "var") 1 1 }
            let (result, finalState) = runCompilerTest (errorAtCurrent "Test error") initialState
            result `shouldBe` Just ()
            hadError finalState `shouldBe` True
            panicMode finalState `shouldBe` True
            debugBuffer finalState `shouldBe` [T.pack "[line 1, col 1] Error at 'var': Test error"]

    describe "errorAtPrevious" $ do
        it "should call errorAt with the previous token" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenIdentifier (T.pack "var") 1 1 }
            let (result, finalState) = runCompilerTest (errorAtPrevious "Test previous error") initialState
            result `shouldBe` Just ()
            hadError finalState `shouldBe` True
            panicMode finalState `shouldBe` True
            debugBuffer finalState `shouldBe` [T.pack "[line 1, col 1] Error at 'var': Test previous error"]

    describe "beginScope" $ do
        it "should increase the scope depth by 1" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerScopeDepth = 0 } }
            let (result, finalState) = runCompilerTest beginScope initialState
            result `shouldBe` Just ()
            compilerScopeDepth (current finalState) `shouldBe` 1

    describe "advance" $ do
        it "should advance to the next token" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenVar (T.pack "var") 1 1 }
            let (result, finalState) = runCompilerTest advance initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenVar (T.pack "var") 1 1

    describe "match" $ do
        it "should return True and advance if the token matches" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenVar (T.pack "var") 1 1 }
            let (result, finalState) = runCompilerTest (match S.TokenVar) initialState
            result `shouldBe` Just True
            currentToken finalState `shouldBe` S.Token S.TokenVar (T.pack "var") 1 1

        it "should return False and not advance if the token does not match" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenIdentifier (T.pack "var") 1 1 }
            let (result, finalState) = runCompilerTest (match S.TokenVar) initialState
            result `shouldBe` Just False
            currentToken finalState `shouldBe` S.Token S.TokenIdentifier (T.pack "var") 1 1

    describe "endScope" $ do
        it "should decrease the scope depth by 1 and emit pops" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerScopeDepth = 1, compilerLocals = [Local (S.Token S.TokenIdentifier (T.pack "x") 1 1) 1 False] } }
            let (result, finalState) = runCompilerTest endScope initialState
            result `shouldBe` Just ()
            compilerScopeDepth (current finalState) `shouldBe` 0
            compilerLocals (current finalState) `shouldBe` []

    describe "addLocal" $ do
        it "should add a local variable to the compiler" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerScopeDepth = 1, compilerLocals = [] } }
            let token = S.Token S.TokenIdentifier (T.pack "x") 1 1
            let (result, finalState) = runCompilerTest (addLocal token) initialState
            result `shouldBe` Just ()
            compilerLocals (current finalState) `shouldBe` [Local token 1 False]

    describe "declareVariable" $ do
        it "should declare a variable in the current scope" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerScopeDepth = 1, compilerLocals = [] }, previous = S.Token S.TokenIdentifier (T.pack "x") 1 1 }
            let (result, finalState) = runCompilerTest declareVariable initialState
            result `shouldBe` Just ()
            compilerLocals (current finalState) `shouldBe` [Local (S.Token S.TokenIdentifier (T.pack "x") 1 1) 1 False]

    describe "resolveLocal" $ do
        it "should resolve a local variable and return its index" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let token = S.Token S.TokenIdentifier (T.pack "x") 1 1
            let initialState = state { current = (current state) { compilerLocals = [Local token 1 False] } }
            let (result, _) = runCompilerTest (resolveLocal token) initialState
            result `shouldBe` Just (Just 1)

        it "should return Nothing if the local variable is not found" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let token = S.Token S.TokenIdentifier (T.pack "y") 1 1
            let initialState = state { current = (current state) { compilerLocals = [Local (S.Token S.TokenIdentifier (T.pack "x") 1 1) 1 False] } }
            let (result, _) = runCompilerTest (resolveLocal token) initialState
            result `shouldBe` Just Nothing

    describe "emitByte" $ do
        it "should emit a byte to the current chunk" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunction = (compilerFunction (current state)) { functionChunk = C.initChunk } } }
            let (result, finalState) = runCompilerTest (emitByte 42) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [42]

    describe "emitBytes" $ do
        it "should emit two bytes to the current chunk" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunction = (compilerFunction (current state)) { functionChunk = C.initChunk } } }
            let (result, finalState) = runCompilerTest (emitBytes 42 43) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [42, 43]

    describe "emitReturn" $ do
        it "should emit a return bytecode" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunctionType = TypeFunction, compilerFunction = (compilerFunction (current state)) { functionChunk = C.initChunk } } }
            let (result, finalState) = runCompilerTest emitReturn initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpNil, C.toByte C.OpReturn]

    describe "endCompiler" $ do
        it "should end the compiler and return the function" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunctionType = TypeFunction, compilerFunction = (compilerFunction (current state)) { functionChunk = C.initChunk } } }
            let (result, _) = runCompilerTest endCompiler initialState
            result `shouldBe` Just (Function 0 0 (Chunk [C.toByte C.OpNil, C.toByte C.OpReturn] [0, 0] []) Nothing)

    describe "expression" $ do
        it "should parse an expression" $ do
            let source = "5 + 3;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunction = (compilerFunction (current state)) { functionChunk = C.initChunk } } }
            let (result, finalState) = runCompilerTest expression initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` []

    describe "emitConstant" $ do
        it "should emit a constant to the current chunk" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunction = (compilerFunction (current state)) { functionChunk = C.initChunk } } }
            let var = Variable VarInteger (VNumber 42.0)
            let (result, finalState) = runCompilerTest (emitConstant var) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpConstant, 0]

    describe "emitVarType" $ do
        it "should emit the correct byte for a simple variable type" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunction = (compilerFunction (current state)) { functionChunk = C.initChunk } } }
            let (result, finalState) = runCompilerTest (emitVarType VarInteger) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.varTypeToWord8 VarInteger]

        it "should emit the correct bytes for a tuple variable type" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunction = (compilerFunction (current state)) { functionChunk = C.initChunk } } }
            let tupleType = VarTuple [VarInteger, VarString]
            let (result, finalState) = runCompilerTest (emitVarType tupleType) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.varTypeToWord8 tupleType, 2, C.varTypeToWord8 VarInteger, C.varTypeToWord8 VarString]

    describe "resolveUpvalue" $ do
        it "should resolve an upvalue and return its index" $ do
            let source = "var x = 5;"
            let enclosingCompiler = initCompiler TypeFunction Nothing
            let state = initTestState (pack source) True True
            let initialState = state { current = (initCompiler TypeFunction (Just enclosingCompiler)) { compilerLocals = [Local (S.Token S.TokenIdentifier (T.pack "x") 1 1) 1 False] } }
            let (result, _) = runCompilerTest (resolveUpvalue (S.Token S.TokenIdentifier (T.pack "x") 1 1)) initialState
            result `shouldBe` Just Nothing

        it "should return Nothing if the upvalue is not found" $ do
            let source = "var x = 5;"
            let enclosingCompiler = initCompiler TypeFunction Nothing
            let state = initTestState (pack source) True True
            let initialState = state { current = (initCompiler TypeFunction (Just enclosingCompiler)) { compilerLocals = [Local (S.Token S.TokenIdentifier (T.pack "y") 1 1) 1 False] } }
            let (result, _) = runCompilerTest (resolveUpvalue (S.Token S.TokenIdentifier (T.pack "x") 1 1)) initialState
            result `shouldBe` Just Nothing

    describe "emitLoop" $ do
        it "should emit a loop bytecode with correct offset" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunction = (compilerFunction (current state)) { functionChunk = C.initChunk { chunkCode = replicate 10 0 } } } }
            let (result, finalState) = runCompilerTest (emitLoop 5) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` replicate 10 0 ++ [C.toByte C.OpLoop, 0, 8]

    describe "emitJump" $ do
        it "should emit a jump bytecode and return the offset" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunction = (compilerFunction (current state)) { functionChunk = C.initChunk } } }
            let (result, finalState) = runCompilerTest (emitJump C.OpJump) initialState
            result `shouldBe` Just 1
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpJump, 0xFF, 0xFF]

    describe "patchJump" $ do
        it "should patch the jump offset correctly" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunction = (compilerFunction (current state)) { functionChunk = C.initChunk { chunkCode = [C.toByte C.OpJump, 0xFF, 0xFF] } } } }
            let (result, finalState) = runCompilerTest (patchJump 0) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [0, 1, 255]

    describe "emitBinaryOperator" $ do
        it "should emit the correct bytecode for addition" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunction = (compilerFunction (current state)) { functionChunk = C.initChunk } } }
            let (result, finalState) = runCompilerTest (emitBinaryOperator S.TokenPlus) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpAdd]

        it "should emit the correct bytecode for subtraction" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunction = (compilerFunction (current state)) { functionChunk = C.initChunk } } }
            let (result, finalState) = runCompilerTest (emitBinaryOperator S.TokenMinus) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpSubtract]

    describe "emitReturn" $ do
        it "should emit a return bytecode for a function" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunctionType = TypeFunction, compilerFunction = (compilerFunction (current state)) { functionChunk = C.initChunk } } }
            let (result, finalState) = runCompilerTest emitReturn initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpNil, C.toByte C.OpReturn]

    describe "checkLocalExists" $ do
        it "should not throw an error if no local variable with the same name exists in the current scope" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let token = S.Token S.TokenIdentifier (T.pack "y") 1 1
            let initialState = state { current = (current state) { compilerLocals = [Local (S.Token S.TokenIdentifier (T.pack "x") 1 1) 1 False] } }
            let (result, finalState) = runCompilerTest (checkLocalExists token) initialState
            result `shouldBe` Just ()
            hadError finalState `shouldBe` False

        it "should throw an error if a local variable with the same name exists in the current scope" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let token = S.Token S.TokenIdentifier (T.pack "x") 1 1
            let initialState = state { current = (current state) { compilerLocals = [Local token 1 False] } }
            let (result, finalState) = runCompilerTest (checkLocalExists token) initialState
            result `shouldBe` Just ()
            hadError finalState `shouldBe` True
            debugBuffer finalState `shouldBe` [T.pack "[line 0, col 0] Error: Already a variable with this name in this scope."]

    describe "resolveLocal" $ do
        it "should resolve a local variable and return its index" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let token = S.Token S.TokenIdentifier (T.pack "x") 1 1
            let initialState = state { current = (current state) { compilerLocals = [Local token 1 False] } }
            let (result, _) = runCompilerTest (resolveLocal token) initialState
            result `shouldBe` Just (Just 1)

        it "should return Nothing if the local variable is not found" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let token = S.Token S.TokenIdentifier (T.pack "y") 1 1
            let initialState = state { current = (current state) { compilerLocals = [Local (S.Token S.TokenIdentifier (T.pack "x") 1 1) 1 False] } }
            let (result, _) = runCompilerTest (resolveLocal token) initialState
            result `shouldBe` Just Nothing

    describe "resolveLocalHelper" $ do
        it "should return Nothing if the list of locals is empty" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let token = S.Token S.TokenIdentifier (T.pack "x") 1 1
            let (result, _) = runCompilerTest (resolveLocalHelper token []) state
            result `shouldBe` Just Nothing

        it "should return the index if the local variable is found and initialized" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let token = S.Token S.TokenIdentifier (T.pack "x") 1 1
            let locals = [(1, Local token 1 False)]
            let (result, _) = runCompilerTest (resolveLocalHelper token locals) state
            result `shouldBe` Just (Just 1)

        it "should report an error if the local variable is found but not initialized" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let token = S.Token S.TokenIdentifier (T.pack "x") 1 1
            let locals = [(1, Local token (-1) False)]
            let (result, finalState) = runCompilerTest (resolveLocalHelper token locals) state
            result `shouldBe` Just Nothing
            hadError finalState `shouldBe` True
            debugBuffer finalState `shouldBe` [T.pack "[line 0, col 0] Error: Can't read local variable in its own initializer."]

    describe "reportUninitializedVariable" $ do
        it "should report an error for uninitialized variable" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenIdentifier (T.pack "x") 1 1 }
            let (result, finalState) = runCompilerTest reportUninitializedVariable initialState
            result `shouldBe` Just ()
            hadError finalState `shouldBe` True
            debugBuffer finalState `shouldBe` [T.pack "[line 1, col 1] Error at 'x': Can't read local variable in its own initializer."]

    describe "markInitialized" $ do
        it "should mark the last local variable as initialized" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let token = S.Token S.TokenIdentifier (T.pack "x") 1 1
            let initialState = state { current = (current state) { compilerScopeDepth = 1, compilerLocals = [Local token (-1) False] } }
            let (result, finalState) = runCompilerTest markInitialized initialState
            result `shouldBe` Just ()
            compilerLocals (current finalState) `shouldBe` [Local token 1 False]

        it "should do nothing if the scope depth is 0" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let token = S.Token S.TokenIdentifier (T.pack "x") 1 1
            let initialState = state { current = (current state) { compilerScopeDepth = 0, compilerLocals = [Local token (-1) False] } }
            let (result, finalState) = runCompilerTest markInitialized initialState
            result `shouldBe` Just ()
            compilerLocals (current finalState) `shouldBe` [Local token (-1) False]

        it "should do nothing if there are no local variables" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerScopeDepth = 1, compilerLocals = [] } }
            let (result, finalState) = runCompilerTest markInitialized initialState
            result `shouldBe` Just ()
            compilerLocals (current finalState) `shouldBe` []

    describe "restoreEnclosingCompiler" $ do
        it "should restore the enclosing compiler if it exists" $ do
            let source = "var x = 5;"
            let enclosingCompiler = initCompiler TypeFunction Nothing
            let state = initTestState (pack source) True True
            let initialState = state { current = initCompiler TypeFunction (Just enclosingCompiler) }
            let (result, finalState) = runCompilerTest restoreEnclosingCompiler initialState
            result `shouldBe` Just ()
            current finalState `shouldBe` enclosingCompiler

        it "should do nothing if there is no enclosing compiler" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = initCompiler TypeFunction Nothing }
            let (result, finalState) = runCompilerTest restoreEnclosingCompiler initialState
            result `shouldBe` Just ()
            current finalState `shouldBe` current initialState

    describe "debugFunctionIfNeeded" $ do
        it "should call debugFunction if debugMode is True and hadError is False" $ do
            let source = "var x = 5;"
            let func = Function 0 0 C.initChunk Nothing
            let state = initTestState (pack source) True True
            let initialState = state { debugMode = True, hadError = False }
            let (result, finalState) = runCompilerTest (debugFunctionIfNeeded func) initialState
            result `shouldBe` Just ()
            debugBuffer finalState `shouldNotBe` []

        it "should not call debugFunction if debugMode is False" $ do
            let source = "var x = 5;"
            let func = Function 0 0 C.initChunk Nothing
            let state = initTestState (pack source) True True
            let initialState = state { debugMode = False, hadError = False }
            let (result, finalState) = runCompilerTest (debugFunctionIfNeeded func) initialState
            result `shouldBe` Just ()
            debugBuffer finalState `shouldBe` []

        it "should not call debugFunction if hadError is True" $ do
            let source = "var x = 5;"
            let func = Function 0 0 C.initChunk Nothing
            let state = initTestState (pack source) True True
            let initialState = state { debugMode = True, hadError = True }
            let (result, finalState) = runCompilerTest (debugFunctionIfNeeded func) initialState
            result `shouldBe` Just ()
            debugBuffer finalState `shouldBe` []

    describe "debugFunction" $ do
        it "should disassemble the function chunk and add to debugBuffer" $ do
            let source = "var x = 5;"
            let func = Function 0 0 C.initChunk Nothing
            let state = initTestState (pack source) True True
            let initialState = state { debugBuffer = [] }
            let (result, finalState) = runCompilerTest (debugFunction func TypeFunction) initialState
            result `shouldBe` Just ()
            debugBuffer finalState `shouldNotBe` []

    describe "getFunctionName" $ do
        it "should return 'script' for TypeScript function type" $ do
            let func = Function 0 0 C.initChunk Nothing
            let name = getFunctionName func TypeScript
            name `shouldBe` T.pack "script"

        it "should return the function name if it exists" $ do
            let func = Function 0 0 C.initChunk (Just (T.pack "myFunction"))
            let name = getFunctionName func TypeFunction
            name `shouldBe` T.pack "myFunction"

        it "should return '<fn>' if the function name does not exist" $ do
            let func = Function 0 0 C.initChunk Nothing
            let name = getFunctionName func TypeFunction
            name `shouldBe` T.pack "<fn>"

    describe "while" $ do
        it "should execute the action while the condition is True" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunction = (compilerFunction (current state)) { functionChunk = C.initChunk } } }
            let condition = do
                    depth <- gets (compilerScopeDepth . current)
                    modify (\s -> s { current = (current s) { compilerScopeDepth = depth + 1 } })
                    return (depth < 1)
            let action = emitByte 42
            let (result, finalState) = runCompilerTest (while condition action) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldNotBe` []

        it "should not execute the action if the condition is False" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunction = (compilerFunction (current state)) { functionChunk = C.initChunk } } }
            let condition = gets (const False)
            let action = emitByte 42
            let (result, finalState) = runCompilerTest (while condition action) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` []

    describe "accumulateVariable" $ do
        it "should accumulate variables while the condition is True" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerScopeDepth = 0 } }
            let condition = do
                    depth <- gets (compilerScopeDepth . current)
                    modify (\s -> s { current = (current s) { compilerScopeDepth = depth + 1 } })
                    return (depth < 1)
            let action = return (Variable VarInteger (VNumber 42.0))
            let acc = return []
            let (result, _) = runCompilerTest (accumulateVariable condition action acc) initialState
            result `shouldBe` Just [Variable VarInteger (VNumber 42.0)]

        it "should stop accumulating when the condition is False" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state
            let condition = gets (const False)
            let action = return (Variable VarInteger (VNumber 42.0))
            let acc = return []
            let (result, _) = runCompilerTest (accumulateVariable condition action acc) initialState
            result `shouldBe` Just []

    describe "accumulateType" $ do
        it "should accumulate types while the condition is True" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerScopeDepth = 0 } }
            let condition = do
                    depth <- gets (compilerScopeDepth . current)
                    modify (\s -> s { current = (current s) { compilerScopeDepth = depth + 1 } })
                    return (depth < 1)
            let action = return VarInteger
            let acc = return []
            let (result, _) = runCompilerTest (accumulateType condition action acc) initialState
            result `shouldBe` Just [VarInteger]

        it "should stop accumulating when the condition is False" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state
            let condition = gets (const False)
            let action = return VarInteger
            let acc = return []
            let (result, _) = runCompilerTest (accumulateType condition action acc) initialState
            result `shouldBe` Just []

    describe "declaration" $ do
        it "should try to match class declaration" $ do
            let source = "class MyClass {}"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenClass (T.pack "class") 1 1 }
            let (result, finalState) = runCompilerTest declaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenEOF T.empty 1 17

        it "should try to match function declaration" $ do
            let source = "fun myFunction() {}"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenFun (T.pack "fun") 1 1 }
            let (result, finalState) = runCompilerTest declaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenEOF T.empty 1 20

        it "should try to match list declaration" $ do
            let source = "var myList = [1, 2, 3];"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenVar (T.pack "var") 1 1 }
            let (result, finalState) = runCompilerTest declaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenVar (T.pack "var") 1 1

        it "should try to match tuple declaration" $ do
            let source = "var myTuple = (1, 'a');"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenVar (T.pack "var") 1 1 }
            let (result, finalState) = runCompilerTest declaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenVar (T.pack "var") 1 1

        it "should try to match variable declaration" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenVar (T.pack "var") 1 1 }
            let (result, finalState) = runCompilerTest declaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenVar (T.pack "var") 1 1

        it "should try to match a statement if no declaration matches" $ do
            let source = "print 5;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenPrint (T.pack "print") 1 1 }
            let (result, finalState) = runCompilerTest declaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenNumber (T.pack "5") 1 7

    describe "tryMatchDecl" $ do
        it "should execute the action if the token matches" $ do
            let source = "class MyClass {}"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenClass (T.pack "class") 1 1 }
            let action = return ()
            let (result, finalState) = runCompilerTest (tryMatchDecl S.TokenClass action) initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenClass (T.pack "class") 1 1

        it "should restore the original state if the token does not match" $ do
            let source = "fun myFunction() {}"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenFun (T.pack "fun") 1 1 }
            let action = return ()
            let (result, finalState) = runCompilerTest (tryMatchDecl S.TokenClass action) initialState
            result `shouldBe` Nothing
            currentToken finalState `shouldBe` S.Token S.TokenFun (T.pack "fun") 1 1

    describe "statement" $ do
        it "should try to match print statement" $ do
            let source = "print 5;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenPrint (T.pack "print") 1 1 }
            let (result, finalState) = runCompilerTest statement initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenNumber (T.pack "5") 1 7

        it "should try to match println statement" $ do
            let source = "println 5;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenPrintLn (T.pack "println") 1 1 }
            let (result, finalState) = runCompilerTest statement initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenNumber (T.pack "5") 1 9

        it "should try to match set recursion limit statement" $ do
            let source = "setRecursionLimit 1000;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenSetRecursionLimit (T.pack "setRecursionLimit") 1 1 }
            let (result, finalState) = runCompilerTest statement initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenNumber (T.pack "1000") 1 19

        it "should try to match for statement" $ do
            let source = "for (var i = 0; i < 10; i = i + 1) {}"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenFor (T.pack "for") 1 1 }
            let (result, finalState) = runCompilerTest statement initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenIdentifier (T.pack "i") 1 17

        it "should try to match if statement" $ do
            let source = "if (true) {}"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenIf (T.pack "if") 1 1 }
            let (result, finalState) = runCompilerTest statement initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenLeftBrace (T.pack "{") 1 11

        it "should try to match return statement" $ do
            let source = "return 5;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenReturn (T.pack "return") 1 1 }
            let (result, finalState) = runCompilerTest statement initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenReturn (T.pack "return") 1 1

        it "should try to match while statement" $ do
            let source = "while (x < 10) { x = x + 1; }"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenWhile (T.pack "while") 1 1 }
            let (result, finalState) = runCompilerTest statement initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenLeftBrace (T.pack "{") 1 16

        it "should try to match block statement" $ do
            let source = "{ var x = 5; }"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenLeftBrace (T.pack "{") 1 1 }
            let (result, finalState) = runCompilerTest statement initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenEOF (T.pack "") 1 15

        it "should try to match expression statement" $ do
            let source = "5 + 3;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenNumber (T.pack "5") 1 1 }
            let (result, _) = runCompilerTest statement initialState
            result `shouldBe` Just ()

    describe "classDeclaration" $ do
        it "should declare a class and setup the class compiler" $ do
            let source = "class MyClass {}"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenClass (T.pack "class") 1 1 }
            let (result, finalState) = runCompilerTest classDeclaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenEOF T.empty 1 17
            currentClass finalState `shouldBe` Nothing

    describe "setupClassCompiler" $ do
        it "should initialize and setup the class compiler" $ do
            let source = "class MyClass {}"
            let state = initTestState (pack source) True True
            let classNameToken = S.Token S.TokenIdentifier (T.pack "MyClass") 1 1
            let initialState = state { currentToken = S.Token S.TokenLeftBrace (T.pack "{") 1 1 }
            let (result, finalState) = runCompilerTest (setupClassCompiler classNameToken (T.pack "MyClass")) initialState
            result `shouldBe` Just ()
            currentClass finalState `shouldBe` Nothing

    describe "initializeClassCompiler" $ do
        it "should initialize the class compiler" $ do
            let source = "class MyClass {}"
            let state = initTestState (pack source) True True
            let initialState = state { currentClass = Nothing }
            let (result, finalState) = runCompilerTest initializeClassCompiler initialState
            result `shouldBe` Just ()
            currentClass finalState `shouldNotBe` Nothing

    describe "parseClassBody" $ do
        it "should parse the class body and emit the correct bytecode" $ do
            let source = "class MyClass { fun method() {} }"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenLeftBrace (T.pack "{") 1 1 }
            let (result, finalState) = runCompilerTest parseClassBody initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenEOF (T.pack "") 1 34

    describe "handleSuperclass" $ do
        it "should handle the superclass if present" $ do
            let source = "class MyClass : SuperClass {}"
            let state = initTestState (pack source) True True
            let classNameToken = S.Token S.TokenIdentifier (T.pack "MyClass") 1 1
            let initialState = state { currentToken = S.Token S.TokenColon (T.pack ":") 1 1 }
            let (result, finalState) = runCompilerTest (handleSuperclass classNameToken (T.pack "MyClass")) initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenClass (T.pack "class") 1 1

    describe "checkInheritance" $ do
        it "should throw an error if a class inherits from itself" $ do
            let source = "class MyClass : MyClass {}"
            let state = initTestState (pack source) True True
            let superClassNameToken = S.Token S.TokenIdentifier (T.pack "MyClass") 1 1
            let initialState = state { currentToken = S.Token S.TokenColon (T.pack ":") 1 1 }
            let (result, finalState) = runCompilerTest (checkInheritance (T.pack "MyClass") superClassNameToken) initialState
            result `shouldBe` Just ()
            hadError finalState `shouldBe` True
            debugBuffer finalState `shouldBe` [T.pack "[line 1, col 1] Error at ':': A class can't inherit from itself."]

    describe "setupSuperclassScope" $ do
        it "should setup the superclass scope" $ do
            let source = "class MyClass : SuperClass {}"
            let state = initTestState (pack source) True True
            let classNameToken = S.Token S.TokenIdentifier (T.pack "MyClass") 1 1
            let initialState = state { currentToken = S.Token S.TokenColon (T.pack ":") 1 1 }
            let (result, finalState) = runCompilerTest (setupSuperclassScope classNameToken) initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenColon (T.pack ":") 1 1

    describe "updateClassCompiler" $ do
        it "should update the class compiler to indicate it has a superclass" $ do
            let source = "class MyClass : SuperClass {}"
            let state = initTestState (pack source) True True
            let classCompiler = ClassCompiler { classHasSuperclass = False, classEnclosing = Nothing }
            let initialState = state { currentClass = Just classCompiler }
            let (result, finalState) = runCompilerTest updateClassCompiler initialState
            result `shouldBe` Just ()
            currentClass finalState `shouldBe` Just classCompiler { classHasSuperclass = True }

    describe "finalizeClassCompiler" $ do
        it "should finalize the class compiler and restore the enclosing compiler" $ do
            let source = "class MyClass {}"
            let state = initTestState (pack source) True True
            let classCompiler = ClassCompiler { classHasSuperclass = False, classEnclosing = Nothing }
            let initialState = state { currentClass = Just classCompiler }
            let (result, finalState) = runCompilerTest finalizeClassCompiler initialState
            result `shouldBe` Just ()
            currentClass finalState `shouldBe` Nothing

    describe "classEndScope" $ do
        it "should decrease the scope depth by 1" $ do
            let source = "class MyClass {}"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerScopeDepth = 1 } }
            let (result, _) = runCompilerTest classEndScope initialState
            result `shouldBe` Just ()

    describe "defaultDebugTime" $ do
        it "should return the correct default debug time" $ do
            defaultDebugTime `shouldBe` UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 0)

    describe "optimizeFunction" $ do
        it "should return optimized function if interpret result is InterpretOk" $ do
            let func = Function 0 0 C.initChunk Nothing
            let env = initVM False defaultDebugTime
            let (result, _) = interpret func env
            result `shouldBe` InterpretRuntimeError
            let (optimizedFunc, _) = runCompilerTest (optimizeFunction func) (initTestState T.empty True True)
            optimizedFunc `shouldBe` Just func

        it "should return the original function if interpret result is not InterpretOk" $ do
            let func = Function 0 0 C.initChunk Nothing
            let env = initVM False defaultDebugTime
            let (result, _) = interpret func env { envStack = [Variable VarAny VNil] }
            result `shouldNotBe` InterpretOk
            let (optimizedFunc, _) = runCompilerTest (optimizeFunction func) (initTestState T.empty True True)
            optimizedFunc `shouldBe` Just func

    describe "handleInterpretOk" $ do
        it "should return adjusted function if source parts are not empty" $ do
            let func = Function 0 0 C.initChunk Nothing
            let env = initVM False defaultDebugTime
            let updatedEnv = env { envStdoutBuffer = [T.pack "output"] }
            let (adjustedFunc, _) = runCompilerTest (handleInterpretOk updatedEnv func) (initTestState T.empty True True)
            adjustedFunc `shouldNotBe` Just func

        it "should return the original function if source parts are empty" $ do
            let func = Function 0 0 C.initChunk Nothing
            let env = initVM False defaultDebugTime
            let updatedEnv = env { envStdoutBuffer = [] }
            let (adjustedFunc, _) = runCompilerTest (handleInterpretOk updatedEnv func) (initTestState T.empty True True)
            adjustedFunc `shouldBe` Just (Function { functionArity = 0, functionUpvalueCount = 0, functionChunk = Chunk { chunkCode = [0, 0, 25, 1, 34], chunkLines = [1, 1, 1, 2, 2], chunkConstants = [Variable { varType = VarString, varValue = VObj (OString $ T.pack "") }] }, functionName = Nothing })

    describe "getStdoutOutput" $ do
        it "should return concatenated stdout buffer as string" $ do
            let env = (initVM False defaultDebugTime) { envStdoutBuffer = [T.pack "output1", T.pack "output2"] }
            getStdoutOutput env `shouldBe` "output1output2"

    describe "getStackTop" $ do
        it "should return the top of the stack if it exists" $ do
            let env = (initVM False defaultDebugTime) { envStack = [Variable VarInteger (VNumber 42.0)] }
            getStackTop env `shouldBe` Just (Variable VarInteger (VNumber 42.0))

        it "should return Nothing if the stack is empty" $ do
            let env = (initVM False defaultDebugTime) { envStack = [] }
            getStackTop env `shouldBe` Nothing

    describe "generateSourceParts" $ do
        it "should generate source parts with stdout output and stack top" $ do
            let stdoutOutput = "output"
            let stackTop = Just (Variable VarInteger (VNumber 42.0))
            generateSourceParts stdoutOutput stackTop `shouldBe` ["print \"output\";", "42;"]

        it "should generate source parts with only stdout output if stack top is Nothing" $ do
            let stdoutOutput = "output"
            let stackTop = Nothing
            generateSourceParts stdoutOutput stackTop `shouldBe` ["print \"output\";"]

    describe "compileAndAdjust" $ do
        it "should compile and adjust the function if not a class method" $ do
            let func = Function 0 0 C.initChunk Nothing
            let sourceParts = ["print \"output\";"]
            let (adjustedFunc, _) = runCompilerTest (compileAndAdjust sourceParts Nothing func) (initTestState T.empty True True)
            adjustedFunc `shouldNotBe` Just func

        it "should return the original function if it is a class method" $ do
            let func = Function 0 0 C.initChunk Nothing
            let sourceParts = ["print \"output\";"]
            let state = (initTestState T.empty True True) { currentClass = Just (ClassCompiler Nothing False) }
            let (adjustedFunc, _) = runCompilerTest (compileAndAdjust sourceParts Nothing func) state
            adjustedFunc `shouldBe` Just func

    describe "adjustChunkCode" $ do
        it "should adjust the chunk code if stack top is Just and no enclosing compiler" $ do
            let func = Function 0 0 (Chunk [1, 2, 3, 4, 5] [] []) Nothing
            let adjustedFunc = adjustChunkCode func (Just (Variable VarInteger (VNumber 42.0))) Nothing
            functionChunk adjustedFunc `shouldBe` Chunk [1, 2, 34] [] []

        it "should return the original function if stack top is Nothing or enclosing compiler exists" $ do
            let func = Function 0 0 (Chunk [1, 2, 3, 4, 5] [] []) Nothing
            let adjustedFunc1 = adjustChunkCode func Nothing Nothing
            let adjustedFunc2 = adjustChunkCode func (Just (Variable VarInteger (VNumber 42.0))) (Just (initCompiler TypeFunction Nothing))
            functionChunk adjustedFunc1 `shouldBe` functionChunk func
            functionChunk adjustedFunc2 `shouldBe` functionChunk func

    describe "getTypeTuples" $ do
        it "should accumulate types for a tuple" $ do
            let source = "[Int, String]"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenLeftSqBrace (T.pack "[") 1 1 }
            let (result, _) = runCompilerTest getTypeTuples initialState
            result `shouldBe` Just [VarAny]

    describe "getElemType" $ do
        it "should return the correct type for a valid token" $ do
            let token = S.Token S.TokenIdentifier (T.pack "Int") 1 1
            let (result, _) = runCompilerTest (getElemType (S.tokenType token) token) (initTestState T.empty True True)
            result `shouldBe` Just VarInteger

        it "should return VarAny and set error for an invalid token" $ do
            let token = S.Token S.TokenIdentifier (T.pack "Invalid") 1 1
            let (result, finalState) = runCompilerTest (getElemType (S.tokenType token) token) (initTestState T.empty True True)
            result `shouldBe` Just VarAny
            hadError finalState `shouldBe` True

    describe "tupleDeclaration" $ do
        it "should declare a tuple and handle initialization" $ do
            let source = "[Int, String] myTuple;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenLeftSqBrace (T.pack "[") 1 1 }
            let (result, finalState) = runCompilerTest tupleDeclaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenComma (T.pack ",") 1 5

    describe "handleTupleInitialization" $ do
        it "should handle tuple initialization with expression" $ do
            let source = "[Int, String] myTuple = (1, \"a\");"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenEqual (T.pack "=") 1 1 }
            let (result, finalState) = runCompilerTest (handleTupleInitialization [VarInteger, VarString] 0) initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenIdentifier (T.pack "myTuple") 1 15

        it "should handle tuple initialization with empty tuple" $ do
            let source = "[Int, String] myTuple;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenSemicolon (T.pack ";") 1 1 }
            let (result, finalState) = runCompilerTest (handleTupleInitialization [VarInteger, VarString] 0) initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenLeftSqBrace (T.pack "[") 1 1

    describe "emitEmptyTuple" $ do
        it "should emit an empty tuple with default values" $ do
            let source = "[Int, String] myTuple;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunction = (compilerFunction (current state)) { functionChunk = C.initChunk } } }
            let (result, finalState) = runCompilerTest (emitEmptyTuple [VarInteger, VarString]) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldNotBe` []

    describe "defaultValue" $ do
        it "should return the correct default value for VarInteger" $ do
            defaultValue VarInteger `shouldBe` Variable VarInteger (VNumber 0.0)

        it "should return the correct default value for VarString" $ do
            defaultValue VarString `shouldBe` Variable VarString (VObj $ OString $ T.pack "")

        it "should return the correct default value for VarBool" $ do
            defaultValue VarBool `shouldBe` Variable VarBool (VBool False)

        it "should return the correct default value for VarDouble" $ do
            defaultValue VarDouble `shouldBe` Variable VarDouble (VNumber 0.0)

        it "should return the correct default value for VarAny" $ do
            defaultValue VarAny `shouldBe` Variable VarAny VNil

    describe "listDeclaration" $ do
        it "should declare a list and handle initialization" $ do
            let source = "[Int] myList;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenLeftSqBrace (T.pack "[") 1 1 }
            let (result, finalState) = runCompilerTest listDeclaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenLeftSqBrace (T.pack "[") 1 1

    describe "handleListInitialization" $ do
        it "should handle list initialization with expression" $ do
            let source = "[Int] myList = [1, 2, 3];"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenEqual (T.pack "=") 1 1 }
            let (result, finalState) = runCompilerTest (handleListInitialization VarInteger 0) initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenIdentifier (T.pack "myList") 1 7

        it "should handle list initialization with empty list" $ do
            let source = "[Int] myList;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenSemicolon (T.pack ";") 1 1 }
            let (result, finalState) = runCompilerTest (handleListInitialization VarInteger 0) initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenLeftSqBrace (T.pack "[") 1 1

    describe "listGetType" $ do
        it "should return the correct type for a valid list type" $ do
            listGetType (T.pack "Int") `shouldBe` Just VarInteger
            listGetType (T.pack "Bool") `shouldBe` Just VarBool
            listGetType (T.pack "String") `shouldBe` Just VarString
            listGetType (T.pack "Double") `shouldBe` Just VarDouble

        it "should return Nothing for an invalid list type" $ do
            listGetType (T.pack "Invalid") `shouldBe` Nothing

    describe "parseList" $ do
        it "should parse a list of variables" $ do
            let source = "[1, 2, 3]"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenNumber (T.pack "1") 1 1 }
            let (result, _) = runCompilerTest parseList initialState
            result `shouldBe` Just [Variable VarInteger (VNumber 1.0)]

    describe "addListElem" $ do
        it "should add a list element" $ do
            let source = "1"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenNumber (T.pack "1") 1 1 }
            let (result, _) = runCompilerTest addListElem initialState
            result `shouldBe` Just (Variable VarInteger (VNumber 1.0))

    describe "addToList" $ do
        it "should add a number to the list" $ do
            let token = S.Token S.TokenNumber (T.pack "1") 1 1
            let (result, _) = runCompilerTest (addToList (S.tokenType token) token) (initTestState T.empty True True)
            result `shouldBe` Just (Variable VarInteger (VNumber 1.0))

        it "should add a boolean true to the list" $ do
            let token = S.Token S.TokenTrue (T.pack "true") 1 1
            let (result, _) = runCompilerTest (addToList (S.tokenType token) token) (initTestState T.empty True True)
            result `shouldBe` Just (Variable VarBool (VBool True))

        it "should add a boolean false to the list" $ do
            let token = S.Token S.TokenFalse (T.pack "false") 1 1
            let (result, _) = runCompilerTest (addToList (S.tokenType token) token) (initTestState T.empty True True)
            result `shouldBe` Just (Variable VarBool (VBool False))

        it "should add a string to the list" $ do
            let token = S.Token S.TokenString (T.pack "hello") 1 1
            let (result, _) = runCompilerTest (addToList (S.tokenType token) token) (initTestState T.empty True True)
            result `shouldBe` Just (Variable VarString (VObj (OString (T.pack "hello"))))

        it "should return VarAny and set error for an invalid token" $ do
            let token = S.Token S.TokenIdentifier (T.pack "invalid") 1 1
            let (result, finalState) = runCompilerTest (addToList (S.tokenType token) token) (initTestState T.empty True True)
            result `shouldBe` Just (Variable VarAny VNil)
            hadError finalState `shouldBe` True

    describe "getVariableType" $ do
        it "should return the correct type for a valid variable type" $ do
            let token = S.Token S.TokenIdentifier (T.pack "Int") 1 1
            let state = (initTestState T.empty True True) { previous = token }
            let (result, _) = runCompilerTest getVariableType state
            result `shouldBe` Just VarAny

        it "should return VarAny and set error for an invalid variable type" $ do
            let token = S.Token S.TokenIdentifier (T.pack "Int") 1 1
            let state = (initTestState T.empty True True) { previous = token }
            let (result, _) = runCompilerTest getVariableType state
            let initialState = (initTestState T.empty True True) { previous = token }
            let (_, finalState') = runCompilerTest getVariableType initialState
            result `shouldBe` Just VarAny
            hadError finalState' `shouldBe` True

    describe "checkVariableType" $ do
        it "should return the correct type if colon is matched" $ do
            let source = ": Int"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenColon (T.pack ":") 1 1 }
            let (result', _) = runCompilerTest checkVariableType initialState
            result' `shouldBe` Just VarAny

        it "should return VarAny if colon is not matched" $ do
            let source = "Int"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenIdentifier (T.pack "Int") 1 1 }
            let (result, _) = runCompilerTest checkVariableType initialState
            result `shouldBe` Just VarAny

    describe "returnStatement" $ do
        it "should throw an error if returning from top-level code" $ do
            let source = "return 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunctionType = TypeScript } }
            let (result, finalState) = runCompilerTest returnStatement initialState
            result `shouldBe` Just ()
            hadError finalState `shouldBe` True
            debugBuffer finalState `shouldBe` [T.pack "[line 0, col 0] Error: Can't return from top-level code."]

        it "should handle return from an initializer without value" $ do
            let source = "return;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunctionType = TypeInitializer } }
            let (result, finalState) = runCompilerTest returnStatement initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpNil, C.toByte C.OpReturn]

        it "should throw an error if returning a value from an initializer" $ do
            let source = "return 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunctionType = TypeInitializer } }
            let (result, finalState) = runCompilerTest returnStatement initialState
            result `shouldBe` Just ()
            hadError finalState `shouldBe` True
            debugBuffer finalState `shouldBe` [T.pack "[line 0, col 0] Error: Can't return a value from an initializer."]

        it "should handle return from a function without value" $ do
            let source = "return;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunctionType = TypeFunction } }
            let (result, finalState) = runCompilerTest returnStatement initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpReturn]

        it "should handle return from a function with value" $ do
            let source = "return 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunctionType = TypeFunction } }
            let (result, finalState) = runCompilerTest returnStatement initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpReturn]

    describe "handleInitializerReturn" $ do
        it "should handle return without value in initializer" $ do
            let source = "return;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenSemicolon (T.pack ";") 1 1 }
            let (result, finalState) = runCompilerTest handleInitializerReturn initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpNil, C.toByte C.OpReturn]

        it "should throw an error if returning a value from an initializer" $ do
            let source = "return 5;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenNumber (T.pack "5") 1 1 }
            let (result, finalState) = runCompilerTest handleInitializerReturn initialState
            result `shouldBe` Just ()
            hadError finalState `shouldBe` True
            debugBuffer finalState `shouldBe` [T.pack "[line 0, col 0] Error: Can't return a value from an initializer."]

    describe "handleFunctionReturn" $ do
        it "should handle return without value in function" $ do
            let source = "return;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenSemicolon (T.pack ";") 1 1 }
            let (result, finalState) = runCompilerTest handleFunctionReturn initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpNil, C.toByte C.OpReturn]

        it "should handle return with value in function" $ do
            let source = "return 5;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenNumber (T.pack "5") 1 1 }
            let (result, finalState) = runCompilerTest handleFunctionReturn initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [0, 0, C.toByte C.OpReturn]

    describe "unary" $ do
        it "should emit OpNegate for TokenMinus" $ do
            let source = "-5;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenMinus (T.pack "-") 1 1 }
            let (result, finalState) = runCompilerTest unary initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpNegate]

        it "should emit OpNot for TokenBang" $ do
            let source = "!true;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenBang (T.pack "!") 1 1 }
            let (result, finalState) = runCompilerTest unary initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpNot]

        it "should do nothing for other tokens" $ do
            let source = "5;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenNumber (T.pack "5") 1 1 }
            let (result, finalState) = runCompilerTest unary initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` []

    describe "binary" $ do
        it "should emit the correct bytecode for addition" $ do
            let source = "5 + 3;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenPlus (T.pack "+") 1 1 }
            let (result, finalState) = runCompilerTest binary initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpAdd]

        it "should emit the correct bytecode for subtraction" $ do
            let source = "5 - 3;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenMinus (T.pack "-") 1 1 }
            let (result, finalState) = runCompilerTest binary initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpSubtract]

        it "should emit the correct bytecode for multiplication" $ do
            let source = "5 * 3;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenStar (T.pack "*") 1 1 }
            let (result, finalState) = runCompilerTest binary initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpMultiply]

        it "should emit the correct bytecode for division" $ do
            let source = "5 / 3;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenSlash (T.pack "/") 1 1 }
            let (result, finalState) = runCompilerTest binary initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpDivide]

        it "should emit the correct bytecode for modulo" $ do
            let source = "5 % 3;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenModulo (T.pack "%") 1 1 }
            let (result, finalState) = runCompilerTest binary initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpModulo]

    describe "emitBinaryOperator" $ do
        it "should emit the correct bytecode for addition" $ do
            let source = "5 + 3;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenPlus (T.pack "+") 1 1 }
            let (result, finalState) = runCompilerTest (emitBinaryOperator S.TokenPlus) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpAdd]

        it "should emit the correct bytecode for subtraction" $ do
            let source = "5 - 3;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenMinus (T.pack "-") 1 1 }
            let (result, finalState) = runCompilerTest (emitBinaryOperator S.TokenMinus) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpSubtract]

        it "should emit the correct bytecode for multiplication" $ do
            let source = "5 * 3;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenStar (T.pack "*") 1 1 }
            let (result, finalState) = runCompilerTest (emitBinaryOperator S.TokenStar) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpMultiply]

        it "should emit the correct bytecode for division" $ do
            let source = "5 / 3;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenSlash (T.pack "/") 1 1 }
            let (result, finalState) = runCompilerTest (emitBinaryOperator S.TokenSlash) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpDivide]

        it "should emit the correct bytecode for modulo" $ do
            let source = "5 % 3;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenModulo (T.pack "%") 1 1 }
            let (result, finalState) = runCompilerTest (emitBinaryOperator S.TokenModulo) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpModulo]

        it "should emit the correct bytecode for bang equal" $ do
            let source = "5 != 3;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenBangEqual (T.pack "!=") 1 1 }
            let (result, finalState) = runCompilerTest (emitBinaryOperator S.TokenBangEqual) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpEqual, C.toByte C.OpNot]

        it "should emit the correct bytecode for equal equal" $ do
            let source = "5 == 3;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenEqualEqual (T.pack "==") 1 1 }
            let (result, finalState) = runCompilerTest (emitBinaryOperator S.TokenEqualEqual) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpEqual]

        it "should emit the correct bytecode for greater" $ do
            let source = "5 > 3;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenGreater (T.pack ">") 1 1 }
            let (result, finalState) = runCompilerTest (emitBinaryOperator S.TokenGreater) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpGreater]

        it "should emit the correct bytecode for greater equal" $ do
            let source = "5 >= 3;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenGreaterEqual (T.pack ">=") 1 1 }
            let (result, finalState) = runCompilerTest (emitBinaryOperator S.TokenGreaterEqual) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpLess, C.toByte C.OpNot]

        it "should emit the correct bytecode for less" $ do
            let source = "5 < 3;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenLess (T.pack "<") 1 1 }
            let (result, finalState) = runCompilerTest (emitBinaryOperator S.TokenLess) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpLess]

        it "should emit the correct bytecode for less equal" $ do
            let source = "5 <= 3;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenLessEqual (T.pack "<=") 1 1 }
            let (result, finalState) = runCompilerTest (emitBinaryOperator S.TokenLessEqual) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpGreater, C.toByte C.OpNot]

        it "should do nothing for other tokens" $ do
            let source = "5;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenNumber (T.pack "5") 1 1 }
            let (result, finalState) = runCompilerTest (emitBinaryOperator S.TokenNumber) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` []

    describe "literal" $ do
        it "should emit OpFalse for TokenFalse" $ do
            let source = "false;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenFalse (T.pack "false") 1 1 }
            let (result, finalState) = runCompilerTest literal initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpFalse]

        it "should emit OpTrue for TokenTrue" $ do
            let source = "true;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenTrue (T.pack "true") 1 1 }
            let (result, finalState) = runCompilerTest literal initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpTrue]

        it "should emit OpNil for TokenNil" $ do
            let source = "nil;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenNil (T.pack "nil") 1 1 }
            let (result, finalState) = runCompilerTest literal initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpNil]

        it "should do nothing for other tokens" $ do
            let source = "5;"
            let state = initTestState (pack source) True True
            let initialState = state { previous = S.Token S.TokenNumber (T.pack "5") 1 1 }
            let (result, finalState) = runCompilerTest literal initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` []

    describe "ternary" $ do
        it "should parse ternary expression and emit correct bytecode" $ do
            let source = "true ? 1 : 0;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenQuestionMark (T.pack "?") 1 1 }
            let (result, finalState) = runCompilerTest ternary initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldNotBe` []

    describe "parseCondition" $ do
        it "should parse condition and emit correct bytecode" $ do
            let source = "(true)"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenLeftParen (T.pack "(") 1 1 }
            let (result, finalState) = runCompilerTest parseCondition initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldNotBe` []

    describe "handleFalseBranch" $ do
        it "should handle false branch and emit correct bytecode" $ do
            let source = "false"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenFalse (T.pack "false") 1 1 }
            let (result, finalState) = runCompilerTest handleFalseBranch initialState
            result `shouldBe` Just 1
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldNotBe` []

    describe "handleTrueBranch" $ do
        it "should handle true branch and emit correct bytecode" $ do
            let source = "true"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenTrue (T.pack "true") 1 1 }
            let (result, finalState) = runCompilerTest (handleTrueBranch 1) initialState
            result `shouldBe` Just 1
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldNotBe` []

    describe "parseElseBranch" $ do
        it "should parse else branch and emit correct bytecode" $ do
            let source = "else"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenElse (T.pack "else") 1 1 }
            let (result, finalState) = runCompilerTest (parseElseBranch 1) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldNotBe` []

    describe "getNumberType" $ do
        it "should return VarInteger for integer numbers" $ do
            let (result, _) = runCompilerTest (getNumberType 5.0) (initTestState T.empty True True)
            result `shouldBe` Just VarInteger

        it "should return VarDouble for non-integer numbers" $ do
            let (result, _) = runCompilerTest (getNumberType 5.5) (initTestState T.empty True True)
            result `shouldBe` Just VarDouble

    describe "listAction" $ do
        it "should handle list get action" $ do
            let source = "myList[0];"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenNumber (T.pack "0") 1 1 }
            let (result, finalState) = runCompilerTest (listAction C.OpGetLocal 0) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [0, 0, C.toByte C.OpGetAtIndex, C.toByte C.OpGetLocal, 0]

        it "should handle list set action" $ do
            let source = "myList[0] = 42;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenNumber (T.pack "0") 1 1 }
            let (result, finalState) = runCompilerTest (listAction C.OpGetLocal 0) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [0, 0, 38, 5, 0]

    describe "listActionGet" $ do
        it "should emit get at index bytecode" $ do
            let source = "myList[0];"
            let state = initTestState (pack source) True True
            let (result, finalState) = runCompilerTest (listActionGet (Variable VarInteger (VNumber 0))) state
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [0, 0, C.toByte C.OpGetAtIndex]

    describe "listActionSet" $ do
        it "should emit set at index bytecode" $ do
            let source = "myList[0] = 42;"
            let state = initTestState (pack source) True True
            let (result, finalState) = runCompilerTest (listActionSet (Variable VarInteger (VNumber 0))) state
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [0, 0, C.toByte C.OpSetAtIndex]

    describe "setOp" $ do
        it "should return the correct set operation for get local" $ do
            setOp C.OpGetLocal `shouldBe` C.OpSetLocal

        it "should return the correct set operation for get upvalue" $ do
            setOp C.OpGetUpvalue `shouldBe` C.OpSetUpvalue

        it "should return the correct set operation for get global" $ do
            setOp C.OpGetGlobal `shouldBe` C.OpSetGlobal

        it "should throw an error for invalid get operation" $ do
            evaluate (setOp C.OpAdd) `shouldThrow` anyErrorCall

    describe "and_" $ do
        it "should emit correct bytecode for and operation" $ do
            let source = "true and false;"
            let state = initTestState (pack source) True True
            let (result, finalState) = runCompilerTest and_ state
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldNotBe` []

    describe "or_" $ do
        it "should emit correct bytecode for or operation" $ do
            let source = "true or false;"
            let state = initTestState (pack source) True True
            let (result, finalState) = runCompilerTest or_ state
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldNotBe` []

    describe "call" $ do
        it "should emit correct bytecode for function call" $ do
            let source = "myFunction();"
            let state = initTestState (pack source) True True
            let (result, finalState) = runCompilerTest call state
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldNotBe` []

    describe "argumentList" $ do
        it "should parse argument list and return correct count" $ do
            let source = "(1, 2, 3)"
            let state = initTestState (pack source) True True
            let (result, finalState) = runCompilerTest argumentList state
            result `shouldBe` Just 1
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` []

    describe "argumentListTail" $ do
        it "should parse argument list tail and return correct count" $ do
            let source = ", 2, 3)"
            let state = initTestState (pack source) True True
            let (result, finalState) = runCompilerTest (argumentListTail 1) state
            result `shouldBe` Just 1
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` []

    describe "dot" $ do
        it "should handle property access" $ do
            let source = "object.property;"
            let state = initTestState (pack source) True True
            let (result, finalState) = runCompilerTest dot state
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldNotBe` []

    describe "handlePropertyCall" $ do
        it "should handle property call" $ do
            let source = "object.method();"
            let state = initTestState (pack source) True True
            let (result, finalState) = runCompilerTest (handlePropertyCall 0) state
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldNotBe` []

    describe "getProperty" $ do
        it "should emit get property bytecode" $ do
            let source = "object.property;"
            let state = initTestState (pack source) True True
            let (result, finalState) = runCompilerTest (getProperty 0) state
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpGetProperty, 0]

    describe "canAssignProperty" $ do
        it "should return True if precedence level is less than or equal to assignment" $ do
            let source = "object.property = value;"
            let state = initTestState (pack source) True True
            let (result, _) = runCompilerTest canAssignProperty state
            result `shouldBe` Just True

        it "should return False if precedence level is greater than assignment" $ do
            let source = "object.property + value;"
            let state = initTestState (pack source) True True
            let (result, _) = runCompilerTest canAssignProperty state
            result `shouldBe` Just True

    describe "setProperty" $ do
        it "should emit set property bytecode" $ do
            let source = "object.property = value;"
            let state = initTestState (pack source) True True
            let (result, finalState) = runCompilerTest (setProperty 0) state
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpSetProperty, 0]

    describe "invokeProperty" $ do
        it "should emit invoke property bytecode" $ do
            let source = "object.method();"
            let state = initTestState (pack source) True True
            let (result, finalState) = runCompilerTest (invokeProperty 0) state
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpInvoke, 0, 1]

    describe "super_" $ do
        it "should handle super call" $ do
            let source = "super.method();"
            let state = initTestState (pack source) True True
            let (result, finalState) = runCompilerTest super_ state
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldNotBe` []

    describe "checkSuperUsage" $ do
        it "should throw an error if super is used outside of a class" $ do
            let source = "super.method();"
            let state = initTestState (pack source) True True
            let (result, finalState) = runCompilerTest (checkSuperUsage Nothing) state
            result `shouldBe` Just ()
            hadError finalState `shouldBe` True

        it "should throw an error if super is used in a class with no superclass" $ do
            let source = "super.method();"
            let state = initTestState (pack source) True True
            let classCompiler = ClassCompiler { classHasSuperclass = False, classEnclosing = Nothing }
            let (result, finalState) = runCompilerTest (checkSuperUsage (Just classCompiler)) state
            result `shouldBe` Just ()
            hadError finalState `shouldBe` True

    describe "superInvoke" $ do
        it "should emit super invoke bytecode" $ do
            let source = "super.method();"
            let state = initTestState (pack source) True True
            let (result, finalState) = runCompilerTest (superInvoke 0) state
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [7, 0, 31, 0, 1]

    describe "superGet" $ do
        it "should emit get super bytecode" $ do
            let source = "super.property;"
            let state = initTestState (pack source) True True
            let (result, finalState) = runCompilerTest (superGet 0) state
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [7, 0, C.toByte C.OpGetSuper, 0]

    describe "this_" $ do
        it "should throw an error if this is used outside of a class" $ do
            let source = "this.property;"
            let state = initTestState (pack source) True True
            let (result, finalState) = runCompilerTest this_ state
            result `shouldBe` Just ()
            hadError finalState `shouldBe` True

        it "should handle this usage inside a class" $ do
            let source = "this.property;"
            let state = initTestState (pack source) True True
            let classCompiler = ClassCompiler { classHasSuperclass = False, classEnclosing = Nothing }
            let initialState = state { currentClass = Just classCompiler }
            let (result, finalState) = runCompilerTest this_ initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldNotBe` []

    describe "addUpvalue" $ do
        it "should return existing upvalue index if found" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let upvalue = Upvalue 0 True
            let initialState = state { current = (current state) { compilerUpvalues = [upvalue] } }
            let (result, _) = runCompilerTest (addUpvalue 0 True) initialState
            result `shouldBe` Just (Just 0)

        it "should add new upvalue if not found" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerUpvalues = [] } }
            let (result, finalState) = runCompilerTest (addUpvalue 0 True) initialState
            result `shouldBe` Just (Just 0)
            compilerUpvalues (current finalState) `shouldBe` [Upvalue 0 True]

    describe "addNewUpvalue" $ do
        it "should add a new upvalue and return its index" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerUpvalues = [] } }
            let (result, finalState) = runCompilerTest (addNewUpvalue [] 0 True) initialState
            result `shouldBe` Just (Just 0)
            compilerUpvalues (current finalState) `shouldBe` [Upvalue 0 True]

        it "should throw an error if there are too many upvalues" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let upvalues = replicate 255 (Upvalue 0 True)
            let initialState = state { current = (current state) { compilerUpvalues = upvalues } }
            let (result, finalState) = runCompilerTest (addNewUpvalue upvalues 0 True) initialState
            result `shouldBe` Just (Just 255)
            hadError finalState `shouldBe` True

    describe "findExistingUpvalue" $ do
        it "should return the index of an existing upvalue" $ do
            let upvalues = [Upvalue 0 True, Upvalue 1 False]
            findExistingUpvalue upvalues 1 False `shouldBe` Just 1

        it "should return Nothing if the upvalue is not found" $ do
            let upvalues = [Upvalue 0 True, Upvalue 1 False]
            findExistingUpvalue upvalues 2 True `shouldBe` Nothing

    describe "markCaptured" $ do
        it "should mark the local variable as captured" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let local = Local (S.Token S.TokenIdentifier (T.pack "x") 1 1) 0 False
            let enclosing = (current state) { compilerLocals = [local] }
            let initialState = state { current = (current state) { compilerLocals = [local] } }
            let (result, finalState) = runCompilerTest (markCaptured 0 enclosing) initialState
            result `shouldBe` Just ()
            compilerLocals (current finalState) `shouldBe` [local { localIsCaptured = True }]

    describe "updateAt" $ do
        it "should update the element at the specified index" $ do
            let l :: [Int]
                l = [1, 2, 3]
            updateAt 1 (+1) l `shouldBe` [1, 3, 3]

        it "should do nothing if the index is out of bounds" $ do
            let l :: [Int]
                l = [1, 2, 3]
            updateAt 3 (+1) l `shouldBe` [1, 2, 3]

    describe "getMapType" $ do
        it "should return the correct key and value types for a valid map type" $ do
            let source = "Int: String"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenIdentifier (T.pack "Int") 1 1 }
            let (result, _) = runCompilerTest getMapType initialState
            result `shouldBe` Just (VarAny, VarAny)

        it "should throw an error if there are more than two types in a map" $ do
            let source = "Int: String: Bool"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenIdentifier (T.pack "Int") 1 1 }
            let (result, finalState) = runCompilerTest getMapType initialState
            result `shouldBe` Just (VarAny, VarAny)
            hadError finalState `shouldBe` True
            debugBuffer finalState `shouldBe` [T.pack "[line 1, col 1] Error at 'Int': Need to have a value type for the map."]

    describe "safeMapDeclaration" $ do
        it "should declare a safe map and handle initialization" $ do
            let source = "[Int: String] myMap;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenLeftSqBrace (T.pack "[") 1 1 }
            let (result, finalState) = runCompilerTest safeMapDeclaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenColon (T.pack ":") 1 5

    describe "mapDeclaration" $ do
        it "should declare a map and handle initialization" $ do
            let source = "[Int: String] myMap;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenLeftSqBrace (T.pack "[") 1 1 }
            let (result, finalState) = runCompilerTest mapDeclaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenColon (T.pack ":") 1 5

    describe "initializeMap" $ do
        it "should initialize a map with the correct key and value types" $ do
            let source = "[Int: String] myMap;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenLeftSqBrace (T.pack "[") 1 1 }
            let (result, finalState) = runCompilerTest (initializeMap False) initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenColon (T.pack ":") 1 5

    describe "handleMapInitialization" $ do
        it "should handle map initialization without equal sign" $ do
            let source = "[Int: String] myMap;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenSemicolon (T.pack ";") 1 1 }
            let (result, finalState) = runCompilerTest (handleMapInitialization False VarInteger VarString 0) initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenLeftSqBrace (T.pack "[") 1 1

        it "should throw an error if map initialization has equal sign" $ do
            let source = "[Int: String] myMap = {};"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenEqual (T.pack "=") 1 1 }
            let (result, finalState) = runCompilerTest (handleMapInitialization False VarInteger VarString 0) initialState
            result `shouldBe` Just ()
            hadError finalState `shouldBe` True
            debugBuffer finalState `shouldBe` [T.pack "[line 1, col 1] Error at '[': Can't initialize a Map currently ...!"]

    describe "Eq instances" $ do
        it "should correctly compare Compiler instances" $ do
            let compiler1 = Compiler Nothing (Function 0 0 C.initChunk Nothing) TypeFunction [] 0 [] 0
            let compiler2 = Compiler Nothing (Function 0 0 C.initChunk Nothing) TypeFunction [] 0 [] 0
            let compiler3 = Compiler (Just compiler1) (Function 1 1 C.initChunk (Just (T.pack "func"))) TypeMethod [Local (S.Token S.TokenIdentifier (T.pack "x") 1 1) 1 False] 1 [Upvalue 0 True] 1
            compiler1 `shouldBe` compiler2
            compiler1 `shouldNotBe` compiler3

        it "should correctly compare CompilerState instances" $ do
            let compiler = Compiler Nothing (Function 0 0 C.initChunk Nothing) TypeFunction [] 0 [] 0
            let sca = S.initScanner (T.pack "")
            let token = S.Token S.TokenError (T.pack "") 0 0
            let state1 = CompilerState compiler sca token token False False Nothing False False []
            let state2 = CompilerState compiler sca token token False False Nothing False False []
            let state3 = CompilerState compiler sca token token True True (Just (ClassCompiler Nothing False)) True True [T.pack "error"]
            state1 `shouldBe` state2
            state1 `shouldNotBe` state3

        it "should correctly compare Local instances" $ do
            let token1 = S.Token S.TokenIdentifier (T.pack "x") 1 1
            let token2 = S.Token S.TokenIdentifier (T.pack "y") 1 1
            let local1 = Local token1 1 False
            let local2 = Local token1 1 False
            let local3 = Local token2 2 True
            local1 `shouldBe` local2
            local1 `shouldNotBe` local3

        it "should correctly compare Upvalue instances" $ do
            let upvalue1 = Upvalue 0 True
            let upvalue2 = Upvalue 0 True
            let upvalue3 = Upvalue 1 False
            upvalue1 `shouldBe` upvalue2
            upvalue1 `shouldNotBe` upvalue3

        it "should correctly compare FunctionType instances" $ do
            TypeFunction `shouldBe` TypeFunction
            TypeFunction `shouldNotBe` TypeInitializer
            TypeInitializer `shouldBe` TypeInitializer
            TypeMethod `shouldBe` TypeMethod
            TypeScript `shouldBe` TypeScript

        it "should correctly compare ClassCompiler instances" $ do
            let classCompiler1 = ClassCompiler Nothing False
            let classCompiler2 = ClassCompiler Nothing False
            let classCompiler3 = ClassCompiler (Just classCompiler1) True
            classCompiler1 `shouldBe` classCompiler2
            classCompiler1 `shouldNotBe` classCompiler3

        it "should correctly compare Precedence instances" $ do
            PrecNone `shouldBe` PrecNone
            PrecAssignment `shouldBe` PrecAssignment
            PrecOr `shouldBe` PrecOr
            PrecAnd `shouldBe` PrecAnd
            PrecEq `shouldBe` PrecEq
            PrecComp `shouldBe` PrecComp
            PrecTerm `shouldBe` PrecTerm
            PrecFactor `shouldBe` PrecFactor
            PrecUnary `shouldBe` PrecUnary
            PrecCall `shouldBe` PrecCall
            PrecPrimary `shouldBe` PrecPrimary
            PrecNone `shouldNotBe` PrecAssignment

    describe "emitPops" $ do
        it "should emit the correct number of OpPop bytecodes" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunction = (compilerFunction (current state)) { functionChunk = C.initChunk } } }
            let (result, finalState) = runCompilerTest (emitPops 3) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` replicate 3 (C.toByte C.OpPop)

    describe "addLocal" $ do
        it "should add a local variable to the compiler" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let token = S.Token S.TokenIdentifier (T.pack "x") 1 1
            let initialState = state { current = (current state) { compilerScopeDepth = 1 } }
            let (result, finalState) = runCompilerTest (addLocal token) initialState
            result `shouldBe` Just ()
            compilerLocals (current finalState) `shouldBe` [Local token 1 False]

        it "should throw an error if there are too many local variables" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let token = S.Token S.TokenIdentifier (T.pack "x") 1 1
            let locals = replicate 256 (Local token 1 False)
            let initialState = state { current = (current state) { compilerLocals = locals } }
            let (result, finalState) = runCompilerTest (addLocal token) initialState
            result `shouldBe` Just ()
            hadError finalState `shouldBe` True
            debugBuffer finalState `shouldBe` [T.pack "[line 0, col 0] Error: Too many local variables in function."]

    describe "emitReturn" $ do
        it "should emit a return bytecode for a function" $ do
            let source = "return;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunctionType = TypeFunction } }
            let (result, finalState) = runCompilerTest emitReturn initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpNil, C.toByte C.OpReturn]

        it "should emit a return bytecode for an initializer" $ do
            let source = "return;"
            let state = initTestState (pack source) True True
            let initialState = state { current = (current state) { compilerFunctionType = TypeInitializer } }
            let (result, finalState) = runCompilerTest emitReturn initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [C.toByte C.OpGetLocal, 0, C.toByte C.OpReturn]

    describe "debugFunctionIfNeeded" $ do
        it "should call debugFunction if debugMode is True and hadError is False" $ do
            let source = "var x = 5;"
            let func = Function 0 0 C.initChunk Nothing
            let state = initTestState (pack source) True True
            let initialState = state { debugMode = True, hadError = False }
            let (result, finalState) = runCompilerTest (debugFunctionIfNeeded func) initialState
            result `shouldBe` Just ()
            debugBuffer finalState `shouldNotBe` []

        it "should not call debugFunction if debugMode is False" $ do
            let source = "var x = 5;"
            let func = Function 0 0 C.initChunk Nothing
            let state = initTestState (pack source) True True
            let initialState = state { debugMode = False, hadError = False }
            let (result, finalState) = runCompilerTest (debugFunctionIfNeeded func) initialState
            result `shouldBe` Just ()
            debugBuffer finalState `shouldBe` []

        it "should not call debugFunction if hadError is True" $ do
            let source = "var x = 5;"
            let func = Function 0 0 C.initChunk Nothing
            let state = initTestState (pack source) True True
            let initialState = state { debugMode = True, hadError = True }
            let (result, finalState) = runCompilerTest (debugFunctionIfNeeded func) initialState
            result `shouldBe` Just ()
            debugBuffer finalState `shouldBe` []

    describe "declaration" $ do
        it "should try to match class declaration" $ do
            let source = "class MyClass {}"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenClass (T.pack "class") 1 1 }
            let (result, finalState) = runCompilerTest declaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenEOF T.empty 1 17

        it "should try to match function declaration" $ do
            let source = "fun myFunction() {}"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenFun (T.pack "fun") 1 1 }
            let (result, finalState) = runCompilerTest declaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenEOF T.empty 1 20

        it "should try to match list declaration" $ do
            let source = "var myList = [1, 2, 3];"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenVar (T.pack "var") 1 1 }
            let (result, finalState) = runCompilerTest declaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenVar (T.pack "var") 1 1

        it "should try to match tuple declaration" $ do
            let source = "var myTuple = (1, 'a');"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenVar (T.pack "var") 1 1 }
            let (result, finalState) = runCompilerTest declaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenVar (T.pack "var") 1 1

        it "should try to match variable declaration" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenVar (T.pack "var") 1 1 }
            let (result, finalState) = runCompilerTest declaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenVar (T.pack "var") 1 1

        it "should try to match a statement if no declaration matches" $ do
            let source = "print 5;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenPrint (T.pack "print") 1 1 }
            let (result, finalState) = runCompilerTest declaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenNumber (T.pack "5") 1 7

    describe "declaration" $ do
        it "should try to match class declaration" $ do
            let source = "class MyClass {}"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenClass (T.pack "class") 1 1 }
            let (result, finalState) = runCompilerTest declaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenEOF T.empty 1 17

        it "should try to match function declaration" $ do
            let source = "fun myFunction() {}"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenFun (T.pack "fun") 1 1 }
            let (result, finalState) = runCompilerTest declaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenEOF T.empty 1 20

        it "should try to match list declaration" $ do
            let source = "var myList = [1, 2, 3];"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenList (T.pack "list") 1 1 }
            let (result, finalState) = runCompilerTest declaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenVar (T.pack "var") 1 1

        it "should try to match tuple declaration" $ do
            let source = "var myTuple = [1, 'a'];"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenTuple (T.pack "tuple") 1 1 }
            let (result, finalState) = runCompilerTest declaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenIdentifier (T.pack "a") 1 20

        it "should try to match safe map declaration" $ do
            let source = "safemap[Int: String] myMap;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenSafeMap (T.pack "safeMap") 1 1 }
            let (result, finalState) = runCompilerTest declaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenColon (T.pack ":") 1 12

        it "should try to match map declaration" $ do
            let source = "map[Int: String] mySafeMap;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenMap (T.pack "map") 1 1 }
            let (result, finalState) = runCompilerTest declaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenColon (T.pack ":") 1 8

        it "should try to match variable declaration" $ do
            let source = "var x = 5;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenVar (T.pack "var") 1 1 }
            let (result, finalState) = runCompilerTest declaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenVar (T.pack "var") 1 1

        it "should try to match a statement if no declaration matches" $ do
            let source = "print 5;"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenPrint (T.pack "print") 1 1 }
            let (result, finalState) = runCompilerTest declaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenNumber (T.pack "5") 1 7

    describe "classDeclaration" $ do
        it "should declare a class and setup the class compiler" $ do
            let source = "class MyClass {}"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenClass (T.pack "class") 1 1 }
            let (result, finalState) = runCompilerTest classDeclaration initialState
            result `shouldBe` Just ()
            currentToken finalState `shouldBe` S.Token S.TokenEOF T.empty 1 17
            currentClass finalState `shouldBe` Nothing

        it "should throw an error if class name is missing" $ do
            let source = "class {}"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenClass (T.pack "class") 1 1 }
            let (result, finalState) = runCompilerTest classDeclaration initialState
            result `shouldBe` Just ()
            hadError finalState `shouldBe` True
            debugBuffer finalState `shouldBe` [T.pack "[line 1, col 1] Error at 'class': Expect class name."]

        it "should emit the correct bytecode for class declaration" $ do
            let source = "class MyClass {}"
            let state = initTestState (pack source) True True
            let initialState = state { currentToken = S.Token S.TokenClass (T.pack "class") 1 1 }
            let (result, finalState) = runCompilerTest classDeclaration initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldBe` [35,0,8,0,5,7,1,32,2,37,3,4]

    describe "setupClassCompiler" $ do
        it "should initialize and setup the class compiler" $ do
            let source = "class MyClass {}"
            let state = initTestState (pack source) True True
            let classNameToken = S.Token S.TokenIdentifier (T.pack "MyClass") 1 1
            let initialState = state { currentToken = S.Token S.TokenLeftBrace (T.pack "{") 1 1 }
            let (result, finalState) = runCompilerTest (setupClassCompiler classNameToken (T.pack "MyClass")) initialState
            result `shouldBe` Just ()
            currentClass finalState `shouldBe` Nothing

        it "should handle superclass if present" $ do
            let source = "class MyClass : SuperClass {}"
            let state = initTestState (pack source) True True
            let classNameToken = S.Token S.TokenIdentifier (T.pack "MyClass") 1 1
            let initialState = state { currentToken = S.Token S.TokenColon (T.pack ":") 1 1 }
            let (result, finalState) = runCompilerTest (setupClassCompiler classNameToken (T.pack "MyClass")) initialState
            result `shouldBe` Just ()
            currentClass finalState `shouldBe` Nothing

        it "should emit correct bytecode for named variable" $ do
            let source = "class MyClass {}"
            let state = initTestState (pack source) True True
            let classNameToken = S.Token S.TokenIdentifier (T.pack "MyClass") 1 1
            let initialState = state { currentToken = S.Token S.TokenLeftBrace (T.pack "{") 1 1 }
            let (result, finalState) = runCompilerTest (setupClassCompiler classNameToken (T.pack "MyClass")) initialState
            result `shouldBe` Just ()
            chunkCode (functionChunk (compilerFunction (current finalState))) `shouldNotBe` []

        it "should finalize the class compiler" $ do
            let source = "class MyClass {}"
            let state = initTestState (pack source) True True
            let classNameToken = S.Token S.TokenIdentifier (T.pack "MyClass") 1 1
            let initialState = state { currentToken = S.Token S.TokenLeftBrace (T.pack "{") 1 1 }
            let (result, finalState) = runCompilerTest (setupClassCompiler classNameToken (T.pack "MyClass")) initialState
            result `shouldBe` Just ()
            currentClass finalState `shouldBe` Nothing

        it "should return the correct structure for each token" $ do
            let rule1 = getRule S.TokenLeftParen
            case rule1 of
                ParseRule grp cll prec ->
                    if isJust grp && isJust cll && prec == PrecCall
                    then return ()
                    else expectationFailure "Failed for TokenLeftParen: Expected Just for both fields and PrecCall."

            let rule2 = getRule S.TokenDot
            case rule2 of
                ParseRule grp cll prec ->
                    if isNothing grp && isJust cll && prec == PrecCall
                    then return ()
                    else expectationFailure "Failed for TokenDot: Expected Nothing for grp and PrecCall."

            let rule3 = getRule S.TokenMinus
            case rule3 of
                ParseRule grp cll prec ->
                    if isJust grp && isJust cll && prec == PrecTerm
                    then return ()
                    else expectationFailure "Failed for TokenMinus: Expected Just for both fields and PrecTerm."

            let rule4 = getRule S.TokenPlus
            case rule4 of
                ParseRule grp cll prec ->
                    if isNothing grp && isJust cll && prec == PrecTerm
                    then return ()
                    else expectationFailure "Failed for TokenPlus: Expected Nothing for grp and PrecTerm."

            let rule5 = getRule S.TokenSlash
            case rule5 of
                ParseRule grp cll prec ->
                    if isNothing grp && isJust cll && prec == PrecFactor
                    then return ()
                    else expectationFailure "Failed for TokenSlash: Expected Nothing for grp and PrecFactor."

            let rule6 = getRule S.TokenStar
            case rule6 of
                ParseRule grp cll prec ->
                    if isNothing grp && isJust cll && prec == PrecFactor
                    then return ()
                    else expectationFailure "Failed for TokenStar: Expected Nothing for grp and PrecFactor."

            let rule7 = getRule S.TokenModulo
            case rule7 of
                ParseRule grp cll prec ->
                    if isNothing grp && isJust cll && prec == PrecTerm
                    then return ()
                    else expectationFailure "Failed for TokenModulo: Expected Nothing for grp and PrecTerm."

            let rule8 = getRule S.TokenBang
            case rule8 of
                ParseRule grp cll prec ->
                    if isJust grp && isNothing cll && prec == PrecNone
                    then return ()
                    else expectationFailure "Failed for TokenBang: Expected Just for grp, Nothing for cll and PrecNone."

            let rule9 = getRule S.TokenBangEqual
            case rule9 of
                ParseRule grp cll prec ->
                    if isNothing grp && isJust cll && prec == PrecEq
                    then return ()
                    else expectationFailure "Failed for TokenBangEqual: Expected Nothing for grp and PrecEq."

            let rule10 = getRule S.TokenEqualEqual
            case rule10 of
                ParseRule grp cll prec ->
                    if isNothing grp && isJust cll && prec == PrecEq
                    then return ()
                    else expectationFailure "Failed for TokenEqualEqual: Expected Nothing for grp and PrecEq."

            let rule11 = getRule S.TokenGreater
            case rule11 of
                ParseRule grp cll prec ->
                    if isNothing grp && isJust cll && prec == PrecComp
                    then return ()
                    else expectationFailure "Failed for TokenGreater: Expected Nothing for grp and PrecComp."

            let rule12 = getRule S.TokenGreaterEqual
            case rule12 of
                ParseRule grp cll prec ->
                    if isNothing grp && isJust cll && prec == PrecComp
                    then return ()
                    else expectationFailure "Failed for TokenGreaterEqual: Expected Nothing for grp and PrecComp."

            let rule13 = getRule S.TokenLess
            case rule13 of
                ParseRule grp cll prec ->
                    if isNothing grp && isJust cll && prec == PrecComp
                    then return ()
                    else expectationFailure "Failed for TokenLess: Expected Nothing for grp and PrecComp."

            let rule14 = getRule S.TokenLessEqual
            case rule14 of
                ParseRule grp cll prec ->
                    if isNothing grp && isJust cll && prec == PrecComp
                    then return ()
                    else expectationFailure "Failed for TokenLessEqual: Expected Nothing for grp and PrecComp."

            let rule15 = getRule S.TokenIdentifier
            case rule15 of
                ParseRule grp cll prec ->
                    if isJust grp && isNothing cll && prec == PrecNone
                    then return ()
                    else expectationFailure "Failed for TokenIdentifier: Expected Just for grp, Nothing for cll and PrecNone."

            let rule16 = getRule S.TokenString
            case rule16 of
                ParseRule grp cll prec ->
                    if isJust grp && isNothing cll && prec == PrecNone
                    then return ()
                    else expectationFailure "Failed for TokenString: Expected Just for grp, Nothing for cll and PrecNone."

            let rule17 = getRule S.TokenNumber
            case rule17 of
                ParseRule grp cll prec ->
                    if isJust grp && isNothing cll && prec == PrecNone
                    then return ()
                    else expectationFailure "Failed for TokenNumber: Expected Just for grp, Nothing for cll and PrecNone."

            let rule18 = getRule S.TokenLeftSqBrace
            case rule18 of
                ParseRule grp cll prec ->
                    if isJust grp && isNothing cll && prec == PrecNone
                    then return ()
                    else expectationFailure "Failed for TokenLeftSqBrace: Expected Just for grp, Nothing for cll and PrecNone."

            let rule19 = getRule S.TokenTernary
            case rule19 of
                ParseRule grp cll prec ->
                    if isJust grp && isNothing cll && prec == PrecNone
                    then return ()
                    else expectationFailure "Failed for TokenTernary: Expected Just for grp, Nothing for cll and PrecNone."

            let rule20 = getRule S.TokenAnd
            case rule20 of
                ParseRule grp cll prec ->
                    if isNothing grp && isJust cll && prec == PrecAnd
                    then return ()
                    else expectationFailure "Failed for TokenAnd: Expected Nothing for grp and PrecAnd."

            let rule21 = getRule S.TokenFalse
            case rule21 of
                ParseRule grp cll prec ->
                    if isJust grp && isNothing cll && prec == PrecNone
                    then return ()
                    else expectationFailure "Failed for TokenFalse: Expected Just for grp, Nothing for cll and PrecNone."

            let rule22 = getRule S.TokenNil
            case rule22 of
                ParseRule grp cll prec ->
                    if isJust grp && isNothing cll && prec == PrecNone
                    then return ()
                    else expectationFailure "Failed for TokenNil: Expected Just for grp, Nothing for cll and PrecNone."

            let rule23 = getRule S.TokenOr
            case rule23 of
                ParseRule grp cll prec ->
                    if isNothing grp && isJust cll && prec == PrecOr
                    then return ()
                    else expectationFailure "Failed for TokenOr: Expected Nothing for grp and PrecOr."

            let rule24 = getRule S.TokenSuper
            case rule24 of
                ParseRule grp cll prec ->
                    if isJust grp && isNothing cll && prec == PrecNone
                    then return ()
                    else expectationFailure "Failed for TokenSuper: Expected Just for grp, Nothing for cll and PrecNone."

            let rule25 = getRule S.TokenThis
            case rule25 of
                ParseRule grp cll prec ->
                    if isJust grp && isNothing cll && prec == PrecNone
                    then return ()
                    else expectationFailure "Failed for TokenThis: Expected Just for grp, Nothing for cll and PrecNone."

            let rule26 = getRule S.TokenTrue
            case rule26 of
                ParseRule grp cll prec ->
                    if isJust grp && isNothing cll && prec == PrecNone
                    then return ()
                    else expectationFailure "Failed for TokenTrue: Expected Just for grp, Nothing for cll and PrecNone."

            let rule27 = getRule S.TokenClass
            case rule27 of
                ParseRule grp cll prec ->
                    if isNothing grp && isNothing cll && prec == PrecNone
                    then return ()
                    else expectationFailure "Failed for invalid token: Expected Nothing for both fields and PrecNone."
