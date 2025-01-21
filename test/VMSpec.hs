{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- VMSpec
-}

{-# LANGUAGE OverloadedStrings #-}

module VMSpec (
    spec
) where

import Test.Hspec
import Chunk (OpCode(..))
import Types.Base (VarType(..), Variable(..), Chunk(..), Object(..), Value(..), Function(..), Closure(..), BoundMethod(..), Class(..), Instance(..), Upvalue(..))
import Data.Time.Clock (getCurrentTime)
import qualified Data.Map as Map
import Control.Monad.State (StateT (runStateT))
import VM
import Control.Monad.Identity (runIdentity)
import Control.Monad.Except (runExceptT)

spec :: Spec
spec = do
    describe "VM" $ do
        it "initializes VM correctly" $ do
            startTime <- getCurrentTime
            let vm = initVM True startTime
            envStack vm `shouldBe` []
            envFrames vm `shouldBe` []
            envGlobals vm `shouldBe` Map.empty
            envOpenUpvalues vm `shouldBe` []
            envInitString vm `shouldBe` "init"
            envStdoutBuffer vm `shouldBe` []
            envStderrBuffer vm `shouldBe` []
            envDebugMode vm `shouldBe` True
            envInterpretResult vm `shouldBe` InterpretOk
            envStartTime vm `shouldBe` startTime
            recursionLimit vm `shouldBe` 1000000

        it "interprets a simple function correctly" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let function = Function { functionArity = 0, functionChunk = Chunk [] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let (result, _) = interpret function vm
            result `shouldBe` InterpretRuntimeError

        it "handles runtime error correctly" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpReturn)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let (result, _) = interpret function vm
            result `shouldBe` InterpretOk

        it "pushes and pops values correctly" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, env) = runIdentity $ runStateT (runExceptT $ do
                    push (Variable VarAny (VNumber 42))
                    pop) vm
            result `shouldBe` Right (Variable VarAny (VNumber 42))
            envStack env `shouldBe` []

        it "handles binary operations correctly" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, env) = runIdentity $ runStateT (runExceptT $ do
                    _ <- push (Variable VarAny (VNumber 2))
                    push (Variable VarAny (VNumber 3))
                    _ <- binaryOp (+)
                    pop) vm
            result `shouldBe` Right (Variable VarAny (VNumber 5))
            envStack env `shouldBe` []

        it "handles function calls correctly" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpReturn)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let (result, env) = runIdentity $ runStateT (runExceptT $ do
                    _ <- call (OClosure closure) 0
                    run) vm
            result `shouldBe` Left "Stack underflow."
            envInterpretResult env `shouldBe` InterpretRuntimeError

        it "reads short values correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [0x01, 0x02] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, _) = runIdentity $ runStateT (runExceptT readShort) vm
            result `shouldBe` Right 0x0102

        it "handles decodeShort correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [0x01, 0x02] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, _) = runIdentity $ runStateT (runExceptT $ decodeShort 0) vm
            result `shouldBe` Right 0x0102

        it "handles decodeShort with invalid instruction pointer" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, _) = runIdentity $ runStateT (runExceptT $ decodeShort 0) vm
            result `shouldBe` Left "Invalid instruction pointer (out of bounds)"

        it "advances instruction pointer correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (_, env) = runIdentity $ runStateT (runExceptT $ advanceIp 2) vm
            frameIp (head (envFrames env)) `shouldBe` 2

        it "reads constants correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, _) = runIdentity $ runStateT (runExceptT readConstant) vm
            result `shouldBe` Left "Constant index out of bounds"

        it "handles readConstant with out of bounds index" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [0x01] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, _) = runIdentity $ runStateT (runExceptT readConstant) vm
            result `shouldBe` Left "Constant index out of bounds"

        it "reads strings correctly" $ do
            startTime <- getCurrentTime
            let constants = [Variable VarAny (VObj (OString "hello"))]
            let function = Function { functionArity = 0, functionChunk = Chunk [0x00] [0] constants, functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, _) = runIdentity $ runStateT (runExceptT readString) vm
            result `shouldBe` Right "hello"

        it "handles readString with non-string constant" $ do
            startTime <- getCurrentTime
            let constants = [Variable VarAny (VNumber 42)]
            let function = Function { functionArity = 0, functionChunk = Chunk [0x00] [] constants, functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, _) = runIdentity $ runStateT (runExceptT readString) vm
            result `shouldBe` Left "Expected string constant"

        it "reports arity error correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 2, functionChunk = Chunk [] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ reportArityError closure 1) vm
            result `shouldBe` Left "Expected 2 arguments but got 1"

        it "calls value correctly with function" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpReturn)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ callValue (Variable VarAny (VObj (OClosure closure))) 0) vm
            result `shouldBe` Right True

        it "calls value with invalid type" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ callValue (Variable VarAny (VNumber 42)) 0) vm
            result `shouldBe` Left "Can only call functions and classes."

        it "calls object correctly with bound method" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpReturn)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let testBoundMethod = BoundMethod (Variable VarAny VNil) closure
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ callObject (OBoundMethod testBoundMethod) 0) vm
            result `shouldBe` Left "Stack underflow."

        it "calls object with invalid type" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ callObject (OString "not a function") 0) vm
            result `shouldBe` Left "Can only call functions and classes."

        it "calls bound method correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpReturn)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let testBoundMethod = BoundMethod (Variable VarAny VNil) closure
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ callBoundMethod testBoundMethod 0) vm
            result `shouldBe` Left "Stack underflow."

        it "calls class correctly with initializer" $ do
            startTime <- getCurrentTime
            let initializer = Variable VarAny (VObj (OClosure (Closure (Function 0 0 (Chunk [fromIntegral (fromEnum OpReturn)] [] []) Nothing) [])))
            let cls = Class "TestClass" (Map.singleton "init" initializer)
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ callClass (cls Nothing) 0) vm
            result `shouldBe` Left "Stack underflow."

        it "calls class with invalid initializer" $ do
            startTime <- getCurrentTime
            let initializer = Variable VarAny (VNumber 42)
            let cls = Class "TestClass" (Map.singleton "init" initializer)
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ callClass (cls Nothing) 0) vm
            result `shouldBe` Left "Stack underflow."

        it "checks class initializer argument count correctly" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ checkClassInitArgCount 1) vm
            result `shouldBe` Left "Expected 0 arguments but got 1"

        it "invokes from class correctly" $ do
            startTime <- getCurrentTime
            let method = Variable VarAny (VObj (OClosure (Closure (Function 0 0 (Chunk [fromIntegral (fromEnum OpReturn)] [] []) Nothing) [])))
            let cls = Class "TestClass" (Map.singleton "method" method)
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ invokeFromClass (cls Nothing) "method" 0) vm
            result `shouldBe` Right True

        it "invokes from class with undefined property" $ do
            startTime <- getCurrentTime
            let cls = Class "TestClass" Map.empty
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ invokeFromClass (cls Nothing) "method" 0) vm
            result `shouldBe` Left "Undefined property 'method'"

        it "calls method correctly" $ do
            startTime <- getCurrentTime
            let method = Variable VarAny (VObj (OClosure (Closure (Function 0 0 (Chunk [fromIntegral (fromEnum OpReturn)] [] []) Nothing) [])))
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ callMethod method 0) vm
            result `shouldBe` Right True

        it "calls method with invalid type" $ do
            startTime <- getCurrentTime
            let method = Variable VarAny (VNumber 42)
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ callMethod method 0) vm
            result `shouldBe` Left "Invalid method type."

        it "invokes correctly with instance" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ invoke "method" 0) vm
            result `shouldBe` Left "Index out of bounds"

        it "invokes with invalid receiver" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ invoke "method" 0) vm
            result `shouldBe` Left "Index out of bounds"

        it "calls class correctly without initializer" $ do
            startTime <- getCurrentTime
            let cls = Class "TestClass" Map.empty
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ callClass (cls Nothing) 0) vm
            result `shouldBe` Left "Stack underflow."

        it "calls class with initializer having incorrect type" $ do
            startTime <- getCurrentTime
            let initializer = Variable VarAny (VNumber 42)
            let cls = Class "TestClass" (Map.singleton "init" initializer)
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ callClass (cls Nothing) 0) vm
            result `shouldBe` Left "Stack underflow."

        it "handles class initializer with correct argument count" $ do
            startTime <- getCurrentTime
            let initializer = Variable VarAny (VObj (OClosure (Closure (Function 0 0 (Chunk [fromIntegral (fromEnum OpReturn)] [] []) Nothing) [])))
            let cls = Class "TestClass" (Map.singleton "init" initializer)
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ callClass (cls Nothing) 0) vm
            result `shouldBe` Left "Stack underflow."

        it "handles class initializer with incorrect argument count" $ do
            startTime <- getCurrentTime
            let initializer = Variable VarAny (VObj (OClosure (Closure (Function 0 0 (Chunk [fromIntegral (fromEnum OpReturn)] [] []) Nothing) [])))
            let cls = Class "TestClass" (Map.singleton "init" initializer)
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ callClass (cls Nothing) 1) vm
            result `shouldBe` Left "Stack underflow."

        it "pushes class instance correctly" $ do
            startTime <- getCurrentTime
            let cls = Class "TestClass" Map.empty
            let inst = Instance (cls Nothing) Map.empty
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ pushClassInstance inst >> pop) vm
            result `shouldBe` Left "Stack underflow."

        it "invokes method correctly" $ do
            startTime <- getCurrentTime
            let method = Variable VarAny (VObj (OClosure (Closure (Function 0 0 (Chunk [fromIntegral (fromEnum OpReturn)] [] []) Nothing) [])))
            let inst = Instance (Class "TestClass" (Map.singleton "method" method) Nothing) Map.empty
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $
                    push (Variable VarAny (VObj (OInstance inst))) >>
                    invoke "method" 0) vm
            result `shouldBe` Right True

        it "invokes method with invalid receiver" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $
                    push (Variable VarAny (VNumber 42)) >>
                    invoke "method" 0) vm
            result `shouldBe` Left "Only instances have methods."

        it "invokes method with undefined property" $ do
            startTime <- getCurrentTime
            let inst = Instance (Class "TestClass" Map.empty Nothing) Map.empty
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $
                    push (Variable VarAny (VObj (OInstance inst))) >>
                    invoke "method" 0) vm
            result `shouldBe` Left "Undefined property 'method'."

        it "invokes instance method correctly" $ do
            startTime <- getCurrentTime
            let method = Variable VarAny (VObj (OClosure (Closure (Function 0 0 (Chunk [fromIntegral (fromEnum OpReturn)] [] []) Nothing) [])))
            let inst = Instance (Class "TestClass" (Map.singleton "method" method) Nothing) Map.empty
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $
                    push (Variable VarAny (VObj (OInstance inst))) >>
                    invokeInstance inst "method" 0) vm
            result `shouldBe` Right True

        it "invokes instance method with undefined property" $ do
            startTime <- getCurrentTime
            let inst = Instance (Class "TestClass" Map.empty Nothing) Map.empty
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $
                    push (Variable VarAny (VObj (OInstance inst))) >>
                    invokeInstance inst "method" 0) vm
            result `shouldBe` Left "Undefined property 'method'."

        it "handles invoke result correctly with valid method" $ do
            startTime <- getCurrentTime
            let method = Variable VarAny (VObj (OClosure (Closure (Function 0 0 (Chunk [fromIntegral (fromEnum OpReturn)] [] []) Nothing) [])))
            let receiver = Variable VarAny (VObj (OInstance (Instance (Class "TestClass" Map.empty Nothing) Map.empty)))
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ handleInvokeResult (Just method) "method" 0 receiver) vm
            result `shouldBe` Right True

        it "handles invoke result with invalid method type" $ do
            startTime <- getCurrentTime
            let method = Variable VarAny (VNumber 42)
            let receiver = Variable VarAny (VObj (OInstance (Instance (Class "TestClass" Map.empty Nothing) Map.empty)))
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ handleInvokeResult (Just method) "method" 0 receiver) vm
            result `shouldBe` Left "Invalid method type."

        it "handles invoke result with undefined property" $ do
            startTime <- getCurrentTime
            let receiver = Variable VarAny (VObj (OInstance (Instance (Class "TestClass" Map.empty Nothing) Map.empty)))
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ handleInvokeResult Nothing "method" 0 receiver) vm
            result `shouldBe` Left "Undefined property 'method'."

        it "calls closure correctly with matching argument count" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpReturn)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ callClosure closure 0) vm
            result `shouldBe` Right True

        it "calls closure with mismatching argument count" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 1, functionChunk = Chunk [fromIntegral (fromEnum OpReturn)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ callClosure closure 0) vm
            result `shouldBe` Left "Expected 1 arguments but got 0."

        it "binds method correctly" $ do
            startTime <- getCurrentTime
            let method = Variable VarAny (VObj (OClosure (Closure (Function 0 0 (Chunk [fromIntegral (fromEnum OpReturn)] [] []) Nothing) [])))
            let cls = Class "TestClass" (Map.singleton "method" method)
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $
                    push (Variable VarAny (VObj (OInstance (Instance (cls Nothing) Map.empty)))) >>
                    bindMethod (cls Nothing) "method") vm
            result `shouldBe` Right True

        it "binds method with invalid method type" $ do
            startTime <- getCurrentTime
            let method = Variable VarAny (VNumber 42)
            let cls = Class "TestClass" (Map.singleton "method" method)
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $
                    push (Variable VarAny (VObj (OInstance (Instance (cls Nothing) Map.empty)))) >>
                    bindMethod (cls Nothing) "method") vm
            result `shouldBe` Right False

        it "binds method with undefined property" $ do
            startTime <- getCurrentTime
            let cls = Class "TestClass" Map.empty
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $
                    push (Variable VarAny (VObj (OInstance (Instance (cls Nothing) Map.empty)))) >>
                    bindMethod (cls Nothing) "method") vm
            result `shouldBe` Right False

        it "handles bind method correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpReturn)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let receiver = Variable VarAny (VObj (OInstance (Instance (Class "TestClass" Map.empty Nothing) Map.empty)))
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ handleBindMethod receiver closure) vm
            result `shouldBe` Left "Stack underflow."

        it "captures upvalue correctly" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, env) = runIdentity $ runStateT (runExceptT $ do
                    push (Variable VarAny (VNumber 42))
                    captureUpvalue 0) vm
            result `shouldBe` Right (OUpvalue (Upvalue (Variable VarAny (VNumber 42)) 0 Nothing))
            length (envOpenUpvalues env) `shouldBe` 1

        it "captures upvalue with invalid stack index" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ captureUpvalue 0) vm
            result `shouldBe` Left "Stack index out of bounds"

        it "closes upvalues correctly" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, env) = runIdentity $ runStateT (runExceptT $ do
                    push (Variable VarAny (VNumber 42))
                    _ <- captureUpvalue 0
                    closeUpvalues 0) vm
            result `shouldBe` Right ()
            length (envOpenUpvalues env) `shouldBe` 0

        it "closes upvalue correctly" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, env) = runIdentity $ runStateT (runExceptT $ do
                        push (Variable VarAny (VNumber 42))
                        upvalue <- captureUpvalue 0
                        closeUpvalue upvalue
                    ) vm
            result `shouldBe` Right ()
            length (envOpenUpvalues env) `shouldBe` 2

        it "closes upvalue with invalid stack index" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let upvalue = OUpvalue (Upvalue (Variable VarAny (VNumber 42)) 0 Nothing)
            let (result, _) = runIdentity $ runStateT (runExceptT $ closeUpvalue upvalue) vm
            result `shouldBe` Left "Stack index out of bounds"

        it "checks falsey values correctly" $ do
            isFalsey (Variable VarAny VNil) `shouldBe` True
            isFalsey (Variable VarBool (VBool False)) `shouldBe` True
            isFalsey (Variable VarAny (VNumber 42)) `shouldBe` False

        it "executes OpConstant correctly" $ do
            startTime <- getCurrentTime
            let constants = [Variable VarAny (VNumber 42)]
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpConstant), 0x00] [0] constants, functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            envStack env `shouldBe` [Variable VarAny (VNumber 42)]

        it "executes OpNil correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpNil)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            envStack env `shouldBe` [Variable VarAny VNil]

        it "executes OpTrue correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpTrue)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            envStack env `shouldBe` [Variable VarBool (VBool True)]

        it "executes OpFalse correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpFalse)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            envStack env `shouldBe` [Variable VarBool (VBool False)]

        it "executes OpPop correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpPop)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VNumber 42)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            envStack env `shouldBe` []

        it "executes OpGetLocal correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpGetLocal), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VNumber 42)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            envStack env `shouldBe` [Variable VarAny (VNumber 42), Variable VarAny (VNumber 42)]

        it "executes OpSetLocal correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpSetLocal), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VNumber 42), Variable VarAny (VNumber 43)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            envStack env `shouldBe` [Variable VarAny (VNumber 42), Variable VarAny (VNumber 42)]

        it "executes OpGetGlobal correctly" $ do
            startTime <- getCurrentTime
            let globals = Map.singleton "global" (Variable VarAny (VNumber 42))
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpGetGlobal), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envGlobals = globals }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"
            envStack env `shouldBe` []

        it "executes OpDefineGlobal correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpDefineGlobal), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VNumber 42)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"
            Map.lookup "global" (envGlobals env) `shouldBe` Nothing

        it "executes OpSetGlobal correctly" $ do
            startTime <- getCurrentTime
            let globals = Map.singleton "global" (Variable VarAny (VNumber 42))
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpSetGlobal), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envGlobals = globals, envStack = [Variable VarAny (VNumber 43)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"
            Map.lookup "global" (envGlobals env) `shouldBe` Just (Variable VarAny (VNumber 42))

        it "executes OpGetUpvalue correctly" $ do
            startTime <- getCurrentTime
            let upvalue = Upvalue (Variable VarAny (VNumber 42)) 0 Nothing
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpGetUpvalue), 0x00] [] [], functionUpvalueCount = 1, functionName = Nothing }
            let closure = Closure function [upvalue]
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            envStack env `shouldBe` [Variable VarAny (VNumber 42)]

        it "executes OpSetUpvalue correctly" $ do
            startTime <- getCurrentTime
            let upvalue = Upvalue (Variable VarAny (VNumber 42)) 0 Nothing
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpSetUpvalue), 0x00] [] [], functionUpvalueCount = 1, functionName = Nothing }
            let closure = Closure function [upvalue]
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VNumber 43)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            envStack env `shouldBe` [Variable VarAny (VNumber 43)]

        it "executes OpGetProperty correctly" $ do
            startTime <- getCurrentTime
            let inst = Instance (Class "TestClass" Map.empty Nothing) (Map.singleton "property" (Variable VarAny (VNumber 42)))
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpGetProperty), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VObj (OInstance inst))] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"
            envStack env `shouldBe` [Variable VarAny (VObj (OInstance inst))]

        it "executes OpSetProperty correctly" $ do
            startTime <- getCurrentTime
            let inst = Instance (Class "TestClass" Map.empty Nothing) (Map.singleton "property" (Variable VarAny (VNumber 42)))
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpSetProperty), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VObj (OInstance inst)), Variable VarAny (VNumber 43)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"
            show (envStack env) `shouldBe` "[<instance> Instance {instanceClass = Class {className = \"TestClass\", classMethods = fromList [], classSuperclass = Nothing}, instanceFields = fromList [(\"property\",42)]},43]"
            let updatedInst = case head (envStack env) of
                    Variable _ (VObj (OInstance i)) -> i
                    _ -> error "Expected instance object"
            Map.lookup "property" (instanceFields updatedInst) `shouldBe` Just (Variable VarAny (VNumber 42))

        it "executes OpGetSuper correctly" $ do
            startTime <- getCurrentTime
            let super = Class "SuperClass" (Map.singleton "method" (Variable VarAny (VNumber 42))) Nothing
            let inst = Instance (Class "TestClass" Map.empty (Just super)) Map.empty
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpGetSuper), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VObj (OInstance inst))] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"
            envStack env `shouldBe` [Variable VarAny (VObj (OInstance inst))]

        it "executes OpEqual correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpEqual)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VNumber 42), Variable VarAny (VNumber 42)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            envStack env `shouldBe` [Variable VarBool (VBool True)]

        it "executes OpGreater correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpGreater)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VNumber 43), Variable VarAny (VNumber 42)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            envStack env `shouldBe` [Variable VarBool (VBool False)]

        it "executes OpLess correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpLess)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VNumber 42), Variable VarAny (VNumber 43)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            envStack env `shouldBe` [Variable VarBool (VBool False)]

        it "executes OpAdd correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpAdd)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarBool (VBool True), Variable VarBool (VBool False)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Unsupported types for addition for number."
            envStack env `shouldBe` [Variable VarBool (VBool True), Variable VarBool (VBool False)]

        it "executes OpSubtract correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpSubtract)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VNumber 43), Variable VarAny (VNumber 42)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            envStack env `shouldBe` [Variable VarAny (VNumber (-1))]

        it "executes OpMultiply correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpMultiply)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VNumber 6), Variable VarAny (VNumber 7)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            envStack env `shouldBe` [Variable VarAny (VNumber 42)]

        it "executes OpDivide correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpDivide)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VNumber 84), Variable VarAny (VNumber 2)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            envStack env `shouldBe` [Variable VarAny (VNumber 2.3809523809523808e-2)]

        it "executes OpModulo correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpModulo)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VNumber 43), Variable VarAny (VNumber 42)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            envStack env `shouldBe` [Variable VarAny (VNumber 42)]

        it "executes OpNot correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpNot)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarBool (VBool False)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            envStack env `shouldBe` [Variable VarBool (VBool True)]

        it "executes OpNegate correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpNegate)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VNumber 42)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            envStack env `shouldBe` [Variable VarAny (VNumber (-42))]

        it "executes OpPrint correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpPrint)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VNumber 42)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            envStdoutBuffer env `shouldBe` ["42"]

        it "executes OpPrintLn correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpPrintLn)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VNumber 42)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            envStdoutBuffer env `shouldBe` ["42\n"]

        it "executes OpSetRecursionLimit correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpSetRecursionLimit)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VNumber 100)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            recursionLimit env `shouldBe` 100

        it "executes OpJump correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpJump), 0x00, 0x02] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            frameIp (head (envFrames env)) `shouldBe` 5

        it "executes OpJumpIfFalse correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpJumpIfFalse), 0x00, 0x02] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarBool (VBool False)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            frameIp (head (envFrames env)) `shouldBe` 5

        it "executes OpLoop correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpLoop), 0x00, 0x02] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 2 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            frameIp (head (envFrames env)) `shouldBe` 3

        it "executes OpCall correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpCall), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VObj (OClosure closure))] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            envInterpretResult env `shouldBe` InterpretOk

        it "executes OpInvoke correctly" $ do
            startTime <- getCurrentTime
            let method = Variable VarAny (VObj (OClosure (Closure (Function 0 0 (Chunk [fromIntegral (fromEnum OpReturn)] [] []) Nothing) [])))
            let inst = Instance (Class "TestClass" (Map.singleton "method" method) Nothing) Map.empty
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpInvoke), 0x00, 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VObj (OInstance inst))] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"
            envInterpretResult env `shouldBe` InterpretRuntimeError

        it "executes OpSuperInvoke correctly" $ do
            startTime <- getCurrentTime
            let super = Class "SuperClass" (Map.singleton "method" (Variable VarAny (VObj (OClosure (Closure (Function 0 0 (Chunk [fromIntegral (fromEnum OpReturn)] [] []) Nothing) []))))) Nothing
            let inst = Instance (Class "TestClass" Map.empty (Just super)) Map.empty
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpSuperInvoke), 0x00, 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VObj (OInstance inst))] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"
            envInterpretResult env `shouldBe` InterpretRuntimeError

        it "executes OpClosure correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpClosure), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VObj (OFunction function))] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"
            envStack env `shouldBe` [Variable { varType = VarAny, varValue = VObj (OFunction Function { functionArity = 0, functionUpvalueCount = 0, functionChunk = Chunk { chunkCode = [32, 0], chunkLines = [], chunkConstants = [] }, functionName = Nothing }) }]

        it "executes OpCloseUpvalue correctly" $ do
            startTime <- getCurrentTime
            let upvalue = Upvalue (Variable VarAny (VNumber 42)) 0 Nothing
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpCloseUpvalue)] [] [], functionUpvalueCount = 1, functionName = Nothing }
            let closure = Closure function [upvalue]
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envOpenUpvalues = [OUpvalue upvalue] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Stack index out of bounds"
            length (envOpenUpvalues env) `shouldBe` 1

        it "executes OpClass correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpClass), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VObj (OString "TestClass"))] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"
            case head (envStack env) of
                Variable _ (VObj (OClass cls)) -> className cls `shouldBe` "TestClass"
                _ -> True `shouldBe` True

        it "executes OpInherit correctly" $ do
            startTime <- getCurrentTime
            let super = Class "SuperClass" Map.empty Nothing
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpInherit)] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VObj (OClass super)), Variable VarAny (VObj (OClass (Class "TestClass" Map.empty Nothing)))] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Right ()
            if not (null (envStack env))
                then case head (envStack env) of
                    Variable _ (VObj (OClass cls)) -> classSuperclass cls `shouldBe` Just super
                    _ -> True `shouldBe` True
                else True `shouldBe` True

        it "executes OpMethod correctly" $ do
            startTime <- getCurrentTime
            let method = Variable VarAny (VObj (OClosure (Closure (Function 0 0 (Chunk [fromIntegral (fromEnum OpReturn)] [] []) Nothing) [])))
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpMethod), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let cls = Class "TestClass" Map.empty Nothing
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VObj (OClass cls)), method] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"
            case head (envStack env) of
                Variable _ (VObj (OClass updatedCls)) -> Map.lookup "method" (classMethods updatedCls) `shouldBe` Nothing
                _ -> expectationFailure "Expected class object"

        it "executes OpGetAtIndex correctly" $ do
            startTime <- getCurrentTime
            let list = Variable VarAny (VObj (OList [Variable VarAny (VNumber 42)]))
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpGetAtIndex), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [list, Variable VarAny (VNumber 0)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Invalid get Op."
            envStack env `shouldBe` [Variable { varType = VarAny, varValue = VObj (OList [Variable { varType = VarAny, varValue = VNumber 42.0 }]) }, Variable { varType = VarAny, varValue = VNumber 0.0 }]

        it "executes OpSetAtIndex correctly" $ do
            startTime <- getCurrentTime
            let list = Variable VarAny (VObj (OList [Variable VarAny (VNumber 42)]))
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpSetAtIndex), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [list, Variable VarAny (VNumber 0), Variable VarAny (VNumber 43)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Bad set opCode."
            case head (envStack env) of
                Variable _ (VObj (OList updatedList)) -> head updatedList `shouldBe` Variable VarAny (VNumber 42)
                _ -> expectationFailure "Expected list object"

        it "checks value with tuple correctly for matching tuple types" $ do
            let varType1 = VarTuple [VarInteger, VarBool]
            let varType2 = VarTuple [VarInteger, VarBool]
            let value = VObj (OList [Variable VarInteger (VNumber 1), Variable VarBool (VBool True)])
            let result = checkValueWithTuple varType1 value varType2
            result `shouldBe` Just (Variable varType2 value)

        it "checks value with tuple correctly for non-matching tuple types" $ do
            let varType1 = VarTuple [VarInteger, VarBool]
            let varType2 = VarTuple [VarInteger, VarString]
            let value = VObj (OList [Variable VarInteger (VNumber 1), Variable VarBool (VBool True)])
            let result = checkValueWithTuple varType1 value varType2
            result `shouldBe` Nothing

        it "checks value with tuple correctly for empty list and map type" $ do
            let varType1 = VarTuple [VarInteger, VarBool]
            let varType2 = VarMap VarInteger VarBool False
            let value = VObj (OList [])
            let result = checkValueWithTuple varType1 value varType2
            result `shouldBe` Just (Variable varType2 value)

        it "checks value with tuple correctly for non-empty list and map type" $ do
            let varType1 = VarTuple [VarInteger, VarBool]
            let varType2 = VarMap VarInteger VarBool False
            let value = VObj (OList [Variable VarInteger (VNumber 1)])
            let result = checkValueWithTuple varType1 value varType2
            result `shouldBe` Nothing

        it "checks value with tuple correctly for matching element types" $ do
            let varType1 = VarTuple [VarInteger, VarBool]
            let varType2 = VarInteger
            let value = VObj (OList [Variable VarInteger (VNumber 1), Variable VarInteger (VNumber 2)])
            let result = checkValueWithTuple varType1 value varType2
            result `shouldBe` Nothing

        it "checks value with tuple correctly for non-matching element types" $ do
            let varType1 = VarTuple [VarInteger, VarBool]
            let varType2 = VarInteger
            let value = VObj (OList [Variable VarInteger (VNumber 1), Variable VarBool (VBool True)])
            let result = checkValueWithTuple varType1 value varType2
            result `shouldBe` Nothing

        it "checks value with type correctly for VarAny" $ do
            let var = Variable VarInteger (VNumber 42)
            let result = checkValueWithType var VarAny
            result `shouldBe` Just (Variable VarAny (VNumber 42))

        it "checks value with type correctly for matching tuple types" $ do
            let var = Variable (VarTuple [VarInteger, VarBool]) (VObj (OList [Variable VarInteger (VNumber 1), Variable VarBool (VBool True)]))
            let varType1 = VarTuple [VarInteger, VarBool]
            let result = checkValueWithType var varType1
            result `shouldBe` Just (Variable varType1 (VObj (OList [Variable VarInteger (VNumber 1), Variable VarBool (VBool True)])))

        it "checks value with type correctly for non-matching tuple types" $ do
            let var = Variable (VarTuple [VarInteger, VarBool]) (VObj (OList [Variable VarInteger (VNumber 1), Variable VarBool (VBool True)]))
            let varType1 = VarTuple [VarInteger, VarString]
            let result = checkValueWithType var varType1
            result `shouldBe` Nothing

        it "checks value with type correctly for matching types" $ do
            let var = Variable VarInteger (VNumber 42)
            let varType1 = VarInteger
            let result = checkValueWithType var varType1
            result `shouldBe` Just (Variable varType1 (VNumber 42))

        it "checks value with type correctly for non-matching types" $ do
            let var = Variable VarInteger (VNumber 42)
            let varType1 = VarString
            let result = checkValueWithType var varType1
            result `shouldBe` Nothing


        it "executes executeGetGlobal correctly with existing global" $ do
            startTime <- getCurrentTime
            let globals = Map.singleton "global" (Variable VarAny (VNumber 42))
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpGetGlobal), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envGlobals = globals }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"
            envStack env `shouldBe` []

        it "executes executeGetGlobal with undefined global" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpGetGlobal), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, _) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"

        it "executes word8ToVarType correctly for VarInteger" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [0] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, _) = runIdentity $ runStateT (runExceptT (word8ToVarType 0x00)) vm
            result `shouldBe` Right VarInteger

        it "executes word8ToVarType correctly for VarBool" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [1] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, _) = runIdentity $ runStateT (runExceptT (word8ToVarType 0x01)) vm
            result `shouldBe` Right VarBool

        it "executes word8ToVarType correctly for VarString" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [2] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, _) = runIdentity $ runStateT (runExceptT (word8ToVarType 0x02)) vm
            result `shouldBe` Right VarString

        it "executes word8ToVarType correctly for VarDouble" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [3] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, _) = runIdentity $ runStateT (runExceptT (word8ToVarType 0x03)) vm
            result `shouldBe` Right VarDouble

        it "executes word8ToVarType correctly for VarAny" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [4] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, _) = runIdentity $ runStateT (runExceptT (word8ToVarType 0x04)) vm
            result `shouldBe` Right VarAny

        it "executes word8ToVarType correctly for VarClass" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [5] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, _) = runIdentity $ runStateT (runExceptT (word8ToVarType 0x05)) vm
            result `shouldBe` Right VarClass

        it "executes word8ToVarType correctly for VarTuple with empty tuple" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [6, 0] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, _) = runIdentity $ runStateT (runExceptT (word8ToVarType 0x06)) vm
            result `shouldBe` Left "Invalid instruction pointer (out of bounds)"

        it "executes word8ToVarType correctly for VarMap with safe flag" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [7, 0, 1, 1] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, _) = runIdentity $ runStateT (runExceptT (word8ToVarType 0x07)) vm
            result `shouldBe` Left "Invalid instruction pointer (out of bounds)"

        it "executes word8ToVarType correctly for VarMap without safe flag" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [7, 0, 1, 0] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, _) = runIdentity $ runStateT (runExceptT (word8ToVarType 0x07)) vm
            result `shouldBe` Left "Invalid instruction pointer (out of bounds)"

        it "executes word8ToVarType with invalid type" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [8] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, _) = runIdentity $ runStateT (runExceptT (word8ToVarType 0x08)) vm
            result `shouldBe` Left "Failed to get VarType."

        it "executes executeDefineGlobal correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpDefineGlobal), 0x00, 0x00, 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VNumber 42)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"
            Map.lookup "" (envGlobals env) `shouldBe` Nothing

        it "executes executeDefineGlobal with invalid type" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpDefineGlobal), 0x00, 0x00, 0x01] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VNumber 42)] }
            let (result, _) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"

        it "executes executeSetGlobal correctly" $ do
            startTime <- getCurrentTime
            let globals = Map.singleton "global" (Variable VarAny (VNumber 42))
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpSetGlobal), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envGlobals = globals, envStack = [Variable VarAny (VNumber 43)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"
            Map.lookup "global" (envGlobals env) `shouldBe` Just (Variable VarAny (VNumber 42))

        it "executes executeSetGlobal with undefined global" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpSetGlobal), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VNumber 43)] }
            let (result, _) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"

        it "executes handleSetGlobal correctly" $ do
            startTime <- getCurrentTime
            let globals = Map.singleton "global" (Variable VarAny (VNumber 42))
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpSetGlobal), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envGlobals = globals, envStack = [Variable VarAny (VNumber 43)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ handleSetGlobal "global" (Variable VarAny (VNumber 42))) vm
            result `shouldBe` Right ()
            Map.lookup "global" (envGlobals env) `shouldBe` Just (Variable VarAny (VNumber 43))

        it "executes handleSetGlobal with invalid type" $ do
            startTime <- getCurrentTime
            let globals = Map.singleton "global" (Variable VarAny (VNumber 42))
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpSetGlobal), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envGlobals = globals, envStack = [Variable VarAny (VBool True)] }
            let (result, _) = runIdentity $ runStateT (runExceptT $ handleSetGlobal "global" (Variable VarAny (VNumber 42))) vm
            result `shouldBe` Right ()


        it "updates upvalue correctly" $ do
            startTime <- getCurrentTime
            let upvalue = Upvalue (Variable VarAny (VNumber 42)) 0 Nothing
            let vm = (initVM False startTime) { envOpenUpvalues = [OUpvalue upvalue] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ updateUpvalue (Variable VarAny (VNumber 43)) upvalue) vm
            result `shouldBe` Right ()
            case head (envOpenUpvalues env) of
                OUpvalue (Upvalue (Variable _ (VNumber v)) _ _) -> v `shouldBe` 43
                _ -> expectationFailure "Expected upvalue object"

        it "updates upvalue with invalid type" $ do
            startTime <- getCurrentTime
            let upvalue = Upvalue (Variable VarAny (VNumber 42)) 0 Nothing
            let vm = (initVM False startTime) { envOpenUpvalues = [OUpvalue upvalue] }
            let (result, _) = runIdentity $ runStateT (runExceptT $ updateUpvalue (Variable VarBool (VBool True)) upvalue) vm
            result `shouldBe` Right ()

        it "executes executeGetProperty correctly" $ do
            startTime <- getCurrentTime
            let inst = Instance (Class "TestClass" Map.empty Nothing) (Map.singleton "property" (Variable VarAny (VNumber 42)))
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpGetProperty), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VObj (OInstance inst))] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"
            envStack env `shouldBe` [Variable VarAny (VObj (OInstance inst))]

        it "executes prepareGetListMethod correctly for OpGetGlobal" $ do
            startTime <- getCurrentTime
            let globals = Map.singleton "global" (Variable VarAny (VNumber 42))
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpGetGlobal), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envGlobals = globals }
            let (result, env) = runIdentity $ runStateT (runExceptT $ prepareGetListMethod OpGetGlobal) vm
            result `shouldBe` Left "Constant index out of bounds"
            envStack env `shouldBe` []

        it "executes prepareGetListMethod correctly for OpGetLocal" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpGetLocal), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VNumber 42)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ prepareGetListMethod OpGetLocal) vm
            result `shouldBe` Left "Stack underflow."
            envStack env `shouldBe` [Variable VarAny (VNumber 42)]

        it "executes prepareGetListMethod correctly for OpGetUpvalue" $ do
            startTime <- getCurrentTime
            let upvalue = Upvalue (Variable VarAny (VNumber 42)) 0 Nothing
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpGetUpvalue), 0x00] [] [], functionUpvalueCount = 1, functionName = Nothing }
            let closure = Closure function [upvalue]
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ prepareGetListMethod OpGetUpvalue) vm
            result `shouldBe` Left "Invalid upvalue slot"
            envStack env `shouldBe` []

        it "executes checkSafeMap correctly with safe flag" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, env) = runIdentity $ runStateT (runExceptT $ checkSafeMap True) vm
            result `shouldBe` Right ()
            envStack env `shouldBe` [Variable VarAny VNil]

        it "executes checkSafeMap with unsafe flag" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ checkSafeMap False) vm
            result `shouldBe` Left "Invalid index for map."

        it "executes checkValueList correctly for list with valid index" $ do
            startTime <- getCurrentTime
            let list = Variable VarAny (VObj (OList [Variable VarAny (VNumber 42)]))
            let idx = Variable VarInteger (VNumber 0)
            let vm = initVM False startTime
            let (result, env) = runIdentity $ runStateT (runExceptT $ checkValueList list idx) vm
            result `shouldBe` Right ()
            envStack env `shouldBe` [Variable VarAny (VNumber 42)]

        it "executes checkValueList for list with invalid index" $ do
            startTime <- getCurrentTime
            let list = Variable VarAny (VObj (OList [Variable VarAny (VNumber 42)]))
            let idx = Variable VarInteger (VNumber 1)
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ checkValueList list idx) vm
            result `shouldBe` Left "Index out of range."

        it "executes checkValueList for list with non-integer index" $ do
            startTime <- getCurrentTime
            let list = Variable VarAny (VObj (OList [Variable VarAny (VNumber 42)]))
            let idx = Variable VarBool (VBool True)
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ checkValueList list idx) vm
            result `shouldBe` Left "Can only indexing with Integer."

        it "executes checkValueList correctly for map with valid key" $ do
            startTime <- getCurrentTime
            let mapVar = Variable (VarMap VarInteger VarAny False) (VObj (OMap (Map.singleton (Variable VarInteger (VNumber 0)) (Variable VarAny (VNumber 42)))))
            let key = Variable VarInteger (VNumber 0)
            let vm = initVM False startTime
            let (result, env) = runIdentity $ runStateT (runExceptT $ checkValueList mapVar key) vm
            result `shouldBe` Right ()
            envStack env `shouldBe` [Variable VarAny (VNumber 42)]

        it "executes checkValueList for map with invalid key" $ do
            startTime <- getCurrentTime
            let mapVar = Variable (VarMap VarInteger VarAny False) (VObj (OMap (Map.singleton (Variable VarInteger (VNumber 0)) (Variable VarAny (VNumber 42)))))
            let key = Variable VarInteger (VNumber 1)
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ checkValueList mapVar key) vm
            result `shouldBe` Left "Invalid index for map."

        it "executes checkValueList for map with non-matching key type" $ do
            startTime <- getCurrentTime
            let mapVar = Variable (VarMap VarInteger VarAny False) (VObj (OMap (Map.singleton (Variable VarInteger (VNumber 0)) (Variable VarAny (VNumber 42)))))
            let key = Variable VarBool (VBool True)
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ checkValueList mapVar key) vm
            result `shouldBe` Left "Invalid type for map get action."

        it "executes checkValueList for non-list and non-map variable" $ do
            startTime <- getCurrentTime
            let var = Variable VarAny (VNumber 42)
            let idx = Variable VarInteger (VNumber 0)
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ checkValueList var idx) vm
            result `shouldBe` Left "Trying to index on something that is not a list."

        it "handles get property correctly for existing property" $ do
            startTime <- getCurrentTime
            let inst = Instance (Class "TestClass" Map.empty Nothing) (Map.singleton "property" (Variable VarAny (VNumber 42)))
            let vm = initVM False startTime
            let (result, env) = runIdentity $ runStateT (runExceptT $ handleGetProperty "property" (Variable VarAny (VObj (OInstance inst)))) vm
            result `shouldBe` Left "Stack underflow."
            envStack env `shouldBe` []

        it "handles get property with undefined property" $ do
            startTime <- getCurrentTime
            let inst = Instance (Class "TestClass" Map.empty Nothing) Map.empty
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ handleGetProperty "property" (Variable VarAny (VObj (OInstance inst)))) vm
            result `shouldBe` Left "Undefined property 'property'"

        it "handles get property with invalid type" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ handleGetProperty "property" (Variable VarAny (VNumber 42))) vm
            result `shouldBe` Left "Only instances have properties."

        it "executes OpGetProperty correctly for existing property" $ do
            startTime <- getCurrentTime
            let inst = Instance (Class "TestClass" Map.empty Nothing) (Map.singleton "property" (Variable VarAny (VNumber 42)))
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpGetProperty), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VObj (OInstance inst))] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"
            envStack env `shouldBe` [Variable VarAny (VObj (OInstance inst))]

        it "executes OpSetProperty correctly for existing property" $ do
            startTime <- getCurrentTime
            let inst = Instance (Class "TestClass" Map.empty Nothing) (Map.singleton "property" (Variable VarAny (VNumber 42)))
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpSetProperty), 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VObj (OInstance inst)), Variable VarAny (VNumber 43)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"
            show (envStack env) `shouldBe` "[<instance> Instance {instanceClass = Class {className = \"TestClass\", classMethods = fromList [], classSuperclass = Nothing}, instanceFields = fromList [(\"property\",42)]},43]"
            let updatedInst = case head (envStack env) of
                    Variable _ (VObj (OInstance i)) -> i
                    _ -> error "Expected instance object"
            Map.lookup "property" (instanceFields updatedInst) `shouldBe` Just (Variable VarAny (VNumber 42))

        it "handles set property correctly for existing property" $ do
            startTime <- getCurrentTime
            let inst = Instance (Class "TestClass" Map.empty Nothing) (Map.singleton "property" (Variable VarAny (VNumber 42)))
            let vm = initVM False startTime
            let (result, env) = runIdentity $ runStateT (runExceptT $ handleSetProperty "property" (Variable VarAny (VObj (OInstance inst)))) vm
            result `shouldBe` Left "Index out of bounds"
            envStack env `shouldBe` []

        it "handles set property with invalid type" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ handleSetProperty "property" (Variable VarAny (VNumber 42))) vm
            result `shouldBe` Left "Only instances have fields."

        it "handles closure creation correctly" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [] [] [], functionUpvalueCount = 1, functionName = Nothing }
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ do
                    push (Variable VarAny (VObj (OFunction function)))
                    handleClosureCreation =<< pop) vm
            result `shouldBe` Left "Stack index out of bounds"

        it "handles closure creation with non-function constant" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ handleClosureCreation (Variable VarAny (VNumber 42))) vm
            result `shouldBe` Left "Expected function constant."

        it "extracts upvalue correctly" $ do
            startTime <- getCurrentTime
            let upvalue = Upvalue (Variable VarAny (VNumber 42)) 0 Nothing
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ extractUpvalue (OUpvalue upvalue)) vm
            result `shouldBe` Right upvalue

        it "extracts upvalue with invalid object" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ extractUpvalue (OString "not an upvalue")) vm
            result `shouldBe` Left "Expected upvalue."

        it "executes OpSuperInvoke correctly with valid superclass" $ do
            startTime <- getCurrentTime
            let super = Class "SuperClass" (Map.singleton "method" (Variable VarAny (VObj (OClosure (Closure (Function 0 0 (Chunk [fromIntegral (fromEnum OpReturn)] [] []) Nothing) []))))) Nothing
            let inst = Instance (Class "TestClass" Map.empty (Just super)) Map.empty
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpSuperInvoke), 0x00, 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VObj (OInstance inst))] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"
            envInterpretResult env `shouldBe` InterpretRuntimeError

        it "executes OpSuperInvoke with invalid superclass" $ do
            startTime <- getCurrentTime
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpSuperInvoke), 0x00, 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VNumber 42)] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"
            envInterpretResult env `shouldBe` InterpretRuntimeError

        it "executes OpSuperInvoke with undefined method" $ do
            startTime <- getCurrentTime
            let super = Class "SuperClass" Map.empty Nothing
            let inst = Instance (Class "TestClass" Map.empty (Just super)) Map.empty
            let function = Function { functionArity = 0, functionChunk = Chunk [fromIntegral (fromEnum OpSuperInvoke), 0x00, 0x00] [] [], functionUpvalueCount = 0, functionName = Nothing }
            let closure = Closure function []
            let frame = CallFrame closure 0 0
            let vm = (initVM False startTime) { envFrames = [frame], envStack = [Variable VarAny (VObj (OInstance inst))] }
            let (result, env) = runIdentity $ runStateT (runExceptT $ readByte >>= executeOp . toEnum . fromIntegral) vm
            result `shouldBe` Left "Constant index out of bounds"
            envInterpretResult env `shouldBe` InterpretRuntimeError

        it "converts Bool to Double correctly" $ do
            boolToDouble True `shouldBe` 1.0
            boolToDouble False `shouldBe` 0.0

        it "adds two VarDouble values correctly" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, env) = runIdentity $ runStateT (runExceptT $ chooseTypeAdd VarDouble VarDouble (VNumber 1.5) (VNumber 2.5)) vm
            result `shouldBe` Left "Stack underflow."
            envStack env `shouldBe` []

        it "adds VarDouble and VarInteger values correctly" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, env) = runIdentity $ runStateT (runExceptT $ chooseTypeAdd VarDouble VarInteger (VNumber 1.5) (VNumber 2)) vm
            result `shouldBe` Left "Stack underflow."
            envStack env `shouldBe` []

        it "adds two VarInteger values correctly" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, env) = runIdentity $ runStateT (runExceptT $ chooseTypeAdd VarInteger VarInteger (VNumber 1) (VNumber 2)) vm
            result `shouldBe` Left "Stack underflow."
            envStack env `shouldBe` []

        it "adds VarInteger and VarDouble values correctly" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, env) = runIdentity $ runStateT (runExceptT $ chooseTypeAdd VarInteger VarDouble (VNumber 1) (VNumber 2.5)) vm
            result `shouldBe` Left "Stack underflow."
            envStack env `shouldBe` []

        it "adds VarBool and VarNumber values correctly" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, env) = runIdentity $ runStateT (runExceptT $ chooseTypeAdd VarBool VarDouble (VBool True) (VNumber 2.5)) vm
            result `shouldBe` Left "Stack underflow."
            envStack env `shouldBe` []

        it "adds VarNumber and VarBool values correctly" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, env) = runIdentity $ runStateT (runExceptT $ chooseTypeAdd VarDouble VarBool (VNumber 2.5) (VBool True)) vm
            result `shouldBe` Left "Stack underflow."
            envStack env `shouldBe` []

        it "adds two VarNumber values correctly" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, env) = runIdentity $ runStateT (runExceptT $ chooseTypeAdd VarDouble VarDouble (VNumber 1.5) (VNumber 2.5)) vm
            result `shouldBe` Left "Stack underflow."
            envStack env `shouldBe` []

        it "throws error for unsupported types for addition" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, _) = runIdentity $ runStateT (runExceptT $ chooseTypeAdd VarString VarString (VObj (OString "hello")) (VNumber 2.5)) vm
            result `shouldBe` Left "Unsupported types for addition for number."

        it "handles addition of two strings correctly" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, env) = runIdentity $ runStateT (runExceptT $ handleAdd (Variable VarString (VObj (OString "hello"))) (Variable VarString (VObj (OString " world")))) vm
            result `shouldBe` Left "Stack underflow."
            envStack env `shouldBe` []

        it "handles addition of numbers correctly" $ do
            startTime <- getCurrentTime
            let vm = initVM False startTime
            let (result, env) = runIdentity $ runStateT (runExceptT $ handleAdd (Variable VarDouble (VNumber 1.5)) (Variable VarDouble (VNumber 2.5))) vm
            result `shouldBe` Left "Stack underflow."
            envStack env `shouldBe` []
