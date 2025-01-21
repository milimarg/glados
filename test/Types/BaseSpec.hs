{-
EPITECH PROJECT, 2025
B-FUN-500-STG-5-2-glados-augustin.grosnon
File description:
BaseSpec
-}

{-# LANGUAGE OverloadedStrings #-}

module Types.BaseSpec (
    spec
) where

import Test.Hspec
import Types.Base
import qualified Data.Map as Map
import Data.Binary (encode, decode, Binary (get), Get)
import Control.Exception (evaluate)
import Data.Binary.Put (putWord8, runPut)
import Data.Binary.Get (runGet)

spec :: Spec
spec = do
    describe "VarType" $ do
        it "should show VarInteger correctly" $
            show VarInteger `shouldBe` "Integer"

        it "should show VarBool correctly" $
            show VarBool `shouldBe` "Bool"

        it "should show VarString correctly" $
            show VarString `shouldBe` "String"

        it "should show VarDouble correctly" $
            show VarDouble `shouldBe` "Double"

        it "should show VarAny correctly" $
            show VarAny `shouldBe` "Any Type"

        it "should show VarClass correctly" $
            show VarClass `shouldBe` "Class"

        it "should show VarTuple correctly" $
            show (VarTuple [VarInteger, VarBool]) `shouldBe` "Tuple of [Integer, Bool, ]"

        it "should show VarMap correctly" $
            show (VarMap VarString VarInteger True) `shouldBe` "SafeMap of [String, Integer]"

        it "should show VarMap correctly" $
            show (VarMap VarString VarInteger False) `shouldBe` "Map of [String, Integer]"

    describe "Variable" $ do
        it "should show Variable correctly" $
            show (Variable VarInteger (VNumber 42)) `shouldBe` "42"

        it "should compare Variables correctly" $
            Variable VarInteger (VNumber 42) `shouldBe` Variable VarInteger (VNumber 42)

    describe "Value" $ do
        it "should show VNil correctly" $
            show VNil `shouldBe` "nil"

        it "should show VBool correctly" $
            show (VBool True) `shouldBe` "true"

        it "should show VNumber correctly" $
            show (VNumber 42.0) `shouldBe` "42"

        it "should show VNumber correctly" $
            show (VNumber 42.5) `shouldBe` "42.5"

    describe "Object" $ do
        it "should show OString correctly" $
            show (OString "Hello") `shouldBe` "Hello"

        it "should show OFunction correctly" $
            show (OFunction (Function 0 0 (Chunk [] [] []) Nothing)) `shouldBe` "<fn> Function {functionArity = 0, functionUpvalueCount = 0, functionChunk = Chunk {chunkCode = [], chunkLines = [], chunkConstants = []}, functionName = Nothing}"

        it "should show OClosure correctly" $
            show (OClosure (Closure (Function 0 0 (Chunk [] [] []) Nothing) [])) `shouldBe` "<closure>"

        it "should show OUpvalue correctly" $
            show (OUpvalue (Upvalue (Variable VarInteger (VNumber 42)) 0 Nothing)) `shouldBe` "<upvalue> Upvalue {upvalueClosed = 42, upvalueLocation = 0, upvalueNext = Nothing}"

        it "should show OClass correctly" $
            show (OClass (Class "Test" Map.empty Nothing)) `shouldBe` "<class> Class {className = \"Test\", classMethods = fromList [], classSuperclass = Nothing}"

        it "should show OInstance correctly" $
            show (OInstance (Instance (Class "Test" Map.empty Nothing) Map.empty)) `shouldBe` "<instance> Instance {instanceClass = Class {className = \"Test\", classMethods = fromList [], classSuperclass = Nothing}, instanceFields = fromList []}"

        it "should show OBoundMethod correctly" $
            show (OBoundMethod (BoundMethod (Variable VarInteger (VNumber 42)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) []))) `shouldBe` "<bound method> BoundMethod {boundReceiver = 42, boundMethod = Closure {closureFunction = Function {functionArity = 0, functionUpvalueCount = 0, functionChunk = Chunk {chunkCode = [], chunkLines = [], chunkConstants = []}, functionName = Nothing}, closureUpvalues = []}}"

        it "should show OList correctly" $
            show (OList [Variable VarInteger (VNumber 42)]) `shouldBe` "<list> [42]"

        it "should show OMap correctly" $
            show (OMap (Map.singleton (Variable VarString (VObj $ OString "key")) (Variable VarInteger (VNumber 42)))) `shouldBe` "<map> fromList [(key,42)]"

    describe "Binary instances" $ do
        it "should encode and decode VarType correctly" $
            (decode . encode) VarInteger `shouldBe` VarInteger

        it "should encode and decode Variable correctly" $
            (decode . encode) (Variable VarInteger (VNumber 42)) `shouldBe` Variable VarInteger (VNumber 42)

        it "should encode and decode Value correctly" $
            (decode . encode) (VNumber 42) `shouldBe` VNumber 42

        it "should encode and decode Object correctly" $
            (decode . encode) (OString "Hello") `shouldBe` OString "Hello"

        it "should encode and decode BoundMethod correctly" $
            (decode . encode) (BoundMethod (Variable VarInteger (VNumber 42)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) [])) `shouldBe` BoundMethod (Variable VarInteger (VNumber 42)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) [])

        it "should encode and decode Class correctly" $
            (decode . encode) (Class "Test" Map.empty Nothing) `shouldBe` Class "Test" Map.empty Nothing

        it "should encode and decode Closure correctly" $
            (decode . encode) (Closure (Function 0 0 (Chunk [] [] []) Nothing) []) `shouldBe` Closure (Function 0 0 (Chunk [] [] []) Nothing) []

        it "should encode and decode Instance correctly" $
            (decode . encode) (Instance (Class "Test" Map.empty Nothing) Map.empty) `shouldBe` Instance (Class "Test" Map.empty Nothing) Map.empty

        it "should encode and decode Upvalue correctly" $
            (decode . encode) (Upvalue (Variable VarInteger (VNumber 42)) 0 Nothing) `shouldBe` Upvalue (Variable VarInteger (VNumber 42)) 0 Nothing

        it "should encode and decode Function correctly" $
            (decode . encode) (Function 0 0 (Chunk [] [] []) Nothing) `shouldBe` Function 0 0 (Chunk [] [] []) Nothing

        it "should encode and decode Chunk correctly" $
            (decode . encode) (Chunk [] [] []) `shouldBe` Chunk [] [] []

        it "should encode and decode VNil correctly" $
            (decode . encode) VNil `shouldBe` VNil

        it "should encode and decode VBool correctly" $
            (decode . encode) (VBool True) `shouldBe` VBool True

        it "should encode and decode VNumber correctly" $
            (decode . encode) (VNumber 42.0) `shouldBe` VNumber 42.0

        it "should encode and decode VObj correctly" $
            (decode . encode) (VObj (OString "Hello")) `shouldBe` VObj (OString "Hello")

        it "should fail to decode unknown Value tag" $
            evaluate (decode (runPut (putWord8 10)) :: Value) `shouldThrow` anyException

        it "should encode and decode OString correctly" $
            (decode . encode) (OString "Hello") `shouldBe` OString "Hello"

        it "should encode and decode OList correctly" $
            (decode . encode) (OList [Variable VarInteger (VNumber 42)]) `shouldBe` OList [Variable VarInteger (VNumber 42)]

        it "should encode and decode OMap correctly" $
            (decode . encode) (OMap (Map.singleton (Variable VarString (VObj $ OString "key")) (Variable VarInteger (VNumber 42)))) `shouldBe` OMap (Map.singleton (Variable VarString (VObj $ OString "key")) (Variable VarInteger (VNumber 42)))

        it "should encode and decode OBoundMethod correctly" $
            (decode . encode) (OBoundMethod (BoundMethod (Variable VarInteger (VNumber 42)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) []))) `shouldBe` OBoundMethod (BoundMethod (Variable VarInteger (VNumber 42)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) []))

        it "should encode and decode OClass correctly" $
            (decode . encode) (OClass (Class "Test" Map.empty Nothing)) `shouldBe` OClass (Class "Test" Map.empty Nothing)

        it "should encode and decode OClosure correctly" $
            (decode . encode) (OClosure (Closure (Function 0 0 (Chunk [] [] []) Nothing) [])) `shouldBe` OClosure (Closure (Function 0 0 (Chunk [] [] []) Nothing) [])

        it "should encode and decode OFunction correctly" $
            (decode . encode) (OFunction (Function 0 0 (Chunk [] [] []) Nothing)) `shouldBe` OFunction (Function 0 0 (Chunk [] [] []) Nothing)

        it "should encode and decode OInstance correctly" $
            (decode . encode) (OInstance (Instance (Class "Test" Map.empty Nothing) Map.empty)) `shouldBe` OInstance (Instance (Class "Test" Map.empty Nothing) Map.empty)

        it "should encode and decode OUpvalue correctly" $
            (decode . encode) (OUpvalue (Upvalue (Variable VarInteger (VNumber 42)) 0 Nothing)) `shouldBe` OUpvalue (Upvalue (Variable VarInteger (VNumber 42)) 0 Nothing)

        it "should fail to decode unknown Object tag" $
            evaluate (decode (runPut (putWord8 10)) :: Object) `shouldThrow` anyException

        it "should encode and decode VarBool correctly" $
            (decode . encode) VarBool `shouldBe` VarBool

        it "should encode and decode VarString correctly" $
            (decode . encode) VarString `shouldBe` VarString

        it "should encode and decode VarDouble correctly" $
            (decode . encode) VarDouble `shouldBe` VarDouble

        it "should encode and decode VarAny correctly" $
            (decode . encode) VarAny `shouldBe` VarAny

        it "should encode and decode VarClass correctly" $
            (decode . encode) VarClass `shouldBe` VarClass

        it "should encode and decode VarTuple correctly" $
            (decode . encode) (VarTuple [VarInteger, VarBool]) `shouldBe` VarTuple [VarInteger, VarBool]

        it "should encode and decode VarMap correctly" $
            (decode . encode) (VarMap VarString VarInteger True) `shouldBe` VarMap VarString VarInteger True

        it "should encode and decode VarMap correctly" $
            (decode . encode) (VarMap VarString VarInteger False) `shouldBe` VarMap VarString VarInteger False

        it "should fail to decode unknown VarType tag" $
            evaluate (decode (runPut (putWord8 10)) :: VarType) `shouldThrow` anyException

        it "should throw an error for an unknown VarType tag" $ do
            let invalidTag = 255
            let encodedData = runPut (putWord8 invalidTag)
            evaluate (runGet (get :: Get VarType) encodedData) `shouldThrow` anyException

    describe "nextEscape" $ do
        it "should handle empty string" $
            nextEscape "" `shouldBe` ""

        it "should handle newline escape" $
            nextEscape "\\n" `shouldBe` "\n"

        it "should handle tab escape" $
            nextEscape "\\t" `shouldBe` "\t"

        it "should handle carriage return escape" $
            nextEscape "\\r" `shouldBe` "\r"

        it "should handle backslash escape" $
            nextEscape "\\\\" `shouldBe` "\\"

        it "should handle double quote escape" $
            nextEscape "\\\"" `shouldBe` "\""

        it "should handle single quote escape" $
            nextEscape "\\'" `shouldBe` "'"

        it "should handle unknown escape sequence" $
            nextEscape "\\x" `shouldBe` "\\x"

        it "should handle mixed escape sequences" $
            nextEscape "Hello\\nWorld\\t!" `shouldBe` "Hello\nWorld\t!"

    describe "Variable Eq instance" $ do
        it "should return True for equal Variables" $
            Variable VarInteger (VNumber 42) `shouldBe` Variable VarInteger (VNumber 42)

        it "should return False for Variables with different types" $
            Variable VarInteger (VNumber 42) `shouldNotBe` Variable VarBool (VBool True)

        it "should return False for Variables with different values" $
            Variable VarInteger (VNumber 42) `shouldNotBe` Variable VarInteger (VNumber 43)

        it "should return False for Variables with different types and values" $
            Variable VarInteger (VNumber 42) `shouldNotBe` Variable VarBool (VBool False)

    describe "VarType Eq instance" $ do
        it "should return True for equal VarTypes" $
            VarInteger `shouldBe` VarInteger

        it "should return False for different VarTypes" $
            VarInteger `shouldNotBe` VarBool

        it "should return True for equal VarTuple" $
            VarTuple [VarInteger, VarBool] `shouldBe` VarTuple [VarInteger, VarBool]

        it "should return False for different VarTuple" $
            VarTuple [VarInteger, VarBool] `shouldNotBe` VarTuple [VarString, VarDouble]

        it "should return True for equal VarMap" $
            VarMap VarString VarInteger True `shouldBe` VarMap VarString VarInteger True

        it "should return False for different VarMap" $
            VarMap VarString VarInteger True `shouldNotBe` VarMap VarString VarBool False

    describe "VarType Ord instance" $ do
        it "should compare VarInteger and VarBool correctly" $
            VarInteger `shouldSatisfy` (< VarBool)

        it "should compare VarString and VarDouble correctly" $
            VarString `shouldSatisfy` (< VarDouble)

        it "should compare VarAny and VarClass correctly" $
            VarAny `shouldSatisfy` (< VarClass)

        it "should compare VarTuple correctly" $
            VarTuple [VarInteger, VarBool] `shouldSatisfy` (< VarTuple [VarString, VarDouble])

        it "should compare VarMap correctly" $
            VarMap VarString VarInteger True `shouldSatisfy` (< VarMap VarString VarBool False)

    describe "Function Binary instance" $ do
        it "should encode and decode Function with no name correctly" $
            (decode . encode) (Function 0 0 (Chunk [] [] []) Nothing) `shouldBe` Function 0 0 (Chunk [] [] []) Nothing

        it "should encode and decode Function with a name correctly" $
            (decode . encode) (Function 1 2 (Chunk [1, 2, 3] [1, 2, 3] [Variable VarInteger (VNumber 42)]) (Just "testFunction")) `shouldBe` Function 1 2 (Chunk [1, 2, 3] [1, 2, 3] [Variable VarInteger (VNumber 42)]) (Just "testFunction")

        it "should encode and decode Function with empty chunk correctly" $
            (decode . encode) (Function 3 4 (Chunk [] [] []) (Just "emptyChunk")) `shouldBe` Function 3 4 (Chunk [] [] []) (Just "emptyChunk")

        it "should encode and decode Function with non-empty chunk correctly" $
            (decode . encode) (Function 5 6 (Chunk [10, 20, 30] [1, 2, 3] [Variable VarBool (VBool True)]) (Just "nonEmptyChunk")) `shouldBe` Function 5 6 (Chunk [10, 20, 30] [1, 2, 3] [Variable VarBool (VBool True)]) (Just "nonEmptyChunk")

        it "should encode and decode Function with multiple upvalues correctly" $
            (decode . encode) (Function 7 8 (Chunk [40, 50, 60] [4, 5, 6] [Variable VarString (VObj $ OString "value")]) (Just "multipleUpvalues")) `shouldBe` Function 7 8 (Chunk [40, 50, 60] [4, 5, 6] [Variable VarString (VObj $ OString "value")]) (Just "multipleUpvalues")

        it "should encode and decode Function with no upvalues correctly" $
            (decode . encode) (Function 9 0 (Chunk [70, 80, 90] [7, 8, 9] [Variable VarDouble (VNumber 3.14)]) (Just "noUpvalues")) `shouldBe` Function 9 0 (Chunk [70, 80, 90] [7, 8, 9] [Variable VarDouble (VNumber 3.14)]) (Just "noUpvalues")

        it "should encode and decode Function with no arity correctly" $
            (decode . encode) (Function 0 10 (Chunk [100, 110, 120] [10, 11, 12] [Variable VarAny VNil]) (Just "noArity")) `shouldBe` Function 0 10 (Chunk [100, 110, 120] [10, 11, 12] [Variable VarAny VNil]) (Just "noArity")

        it "should encode and decode Function with maximum arity correctly" $
            (decode . encode) (Function maxBound 11 (Chunk [130, 140, 150] [13, 14, 15] [Variable VarClass (VObj $ OClass (Class "TestClass" Map.empty Nothing))]) (Just "maxArity")) `shouldBe` Function maxBound 11 (Chunk [130, 140, 150] [13, 14, 15] [Variable VarClass (VObj $ OClass (Class "TestClass" Map.empty Nothing))]) (Just "maxArity")

        it "should encode and decode Function with no name and no upvalues correctly" $
            (decode . encode) (Function 12 0 (Chunk [160, 170, 180] [16, 17, 18] [Variable (VarTuple [VarInteger, VarBool]) VNil]) Nothing) `shouldBe` Function 12 0 (Chunk [160, 170, 180] [16, 17, 18] [Variable (VarTuple [VarInteger, VarBool]) VNil]) Nothing

        it "should encode and decode Function with complex chunk correctly" $
            (decode . encode) (Function 13 14 (Chunk [190, 200, 210] [19, 20, 21] [Variable (VarMap VarString VarInteger True) VNil]) (Just "complexChunk")) `shouldBe` Function 13 14 (Chunk [190, 200, 210] [19, 20, 21] [Variable (VarMap VarString VarInteger True) VNil]) (Just "complexChunk")

    describe "BoundMethod Eq instance" $ do
        it "should return True for equal BoundMethods" $
            BoundMethod (Variable VarInteger (VNumber 42)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) []) `shouldBe` BoundMethod (Variable VarInteger (VNumber 42)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) [])

        it "should return False for BoundMethods with different receivers" $
            BoundMethod (Variable VarInteger (VNumber 42)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) []) `shouldNotBe` BoundMethod (Variable VarBool (VBool True)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) [])

        it "should return False for BoundMethods with different methods" $
            BoundMethod (Variable VarInteger (VNumber 42)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) []) `shouldNotBe` BoundMethod (Variable VarInteger (VNumber 42)) (Closure (Function 1 0 (Chunk [] [] []) Nothing) [])

    describe "Class Eq instance" $ do
        it "should return True for equal Classes" $
            Class "Test" Map.empty Nothing `shouldBe` Class "Test" Map.empty Nothing

        it "should return False for Classes with different names" $
            Class "Test1" Map.empty Nothing `shouldNotBe` Class "Test2" Map.empty Nothing

        it "should return False for Classes with different methods" $
            Class "Test" (Map.singleton "method" (Variable VarInteger (VNumber 42))) Nothing `shouldNotBe` Class "Test" Map.empty Nothing

        it "should return False for Classes with different superclasses" $
            Class "Test" Map.empty (Just (Class "Super" Map.empty Nothing)) `shouldNotBe` Class "Test" Map.empty Nothing

    describe "Closure Eq instance" $ do
        it "should return True for equal Closures" $
            Closure (Function 0 0 (Chunk [] [] []) Nothing) [] `shouldBe` Closure (Function 0 0 (Chunk [] [] []) Nothing) []

        it "should return False for Closures with different functions" $
            Closure (Function 0 0 (Chunk [] [] []) Nothing) [] `shouldNotBe` Closure (Function 1 0 (Chunk [] [] []) Nothing) []

        it "should return False for Closures with different upvalues" $
            Closure (Function 0 0 (Chunk [] [] []) Nothing) [Upvalue (Variable VarInteger (VNumber 42)) 0 Nothing] `shouldNotBe` Closure (Function 0 0 (Chunk [] [] []) Nothing) []

    describe "Instance Eq instance" $ do
        it "should return True for equal Instances" $
            Instance (Class "Test" Map.empty Nothing) Map.empty `shouldBe` Instance (Class "Test" Map.empty Nothing) Map.empty

        it "should return False for Instances with different classes" $
            Instance (Class "Test1" Map.empty Nothing) Map.empty `shouldNotBe` Instance (Class "Test2" Map.empty Nothing) Map.empty

        it "should return False for Instances with different fields" $
            Instance (Class "Test" Map.empty Nothing) (Map.singleton "field" (Variable VarInteger (VNumber 42))) `shouldNotBe` Instance (Class "Test" Map.empty Nothing) Map.empty

    describe "Upvalue Eq instance" $ do
        it "should return True for equal Upvalues" $
            Upvalue (Variable VarInteger (VNumber 42)) 0 Nothing `shouldBe` Upvalue (Variable VarInteger (VNumber 42)) 0 Nothing

        it "should return False for Upvalues with different closed variables" $
            Upvalue (Variable VarInteger (VNumber 42)) 0 Nothing `shouldNotBe` Upvalue (Variable VarBool (VBool True)) 0 Nothing

        it "should return False for Upvalues with different locations" $
            Upvalue (Variable VarInteger (VNumber 42)) 0 Nothing `shouldNotBe` Upvalue (Variable VarInteger (VNumber 42)) 1 Nothing

        it "should return False for Upvalues with different next upvalues" $
            Upvalue (Variable VarInteger (VNumber 42)) 0 (Just (Upvalue (Variable VarBool (VBool True)) 1 Nothing)) `shouldNotBe` Upvalue (Variable VarInteger (VNumber 42)) 0 Nothing

    describe "Function Eq instance" $ do
        it "should return True for equal Functions" $
            Function 0 0 (Chunk [] [] []) Nothing `shouldBe` Function 0 0 (Chunk [] [] []) Nothing

        it "should return False for Functions with different arities" $
            Function 0 0 (Chunk [] [] []) Nothing `shouldNotBe` Function 1 0 (Chunk [] [] []) Nothing

        it "should return False for Functions with different upvalue counts" $
            Function 0 0 (Chunk [] [] []) Nothing `shouldNotBe` Function 0 1 (Chunk [] [] []) Nothing

        it "should return False for Functions with different chunks" $
            Function 0 0 (Chunk [] [] []) Nothing `shouldNotBe` Function 0 0 (Chunk [1] [1] [Variable VarInteger (VNumber 42)]) Nothing

        it "should return False for Functions with different names" $
            Function 0 0 (Chunk [] [] []) (Just "name1") `shouldNotBe` Function 0 0 (Chunk [] [] []) (Just "name2")

    describe "BoundMethod Ord instance" $ do
        it "should compare BoundMethods correctly" $
            BoundMethod (Variable VarInteger (VNumber 42)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) []) `shouldSatisfy` (< BoundMethod (Variable VarInteger (VNumber 43)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) []))

    describe "Class Ord instance" $ do
        it "should compare Classes correctly" $
            Class "A" Map.empty Nothing `shouldSatisfy` (< Class "B" Map.empty Nothing)

    describe "Closure Ord instance" $ do
        it "should compare Closures correctly" $
            Closure (Function 0 0 (Chunk [] [] []) Nothing) [] `shouldSatisfy` (< Closure (Function 1 0 (Chunk [] [] []) Nothing) [])

    describe "Instance Ord instance" $ do
        it "should compare Instances correctly" $
            Instance (Class "A" Map.empty Nothing) Map.empty `shouldSatisfy` (< Instance (Class "B" Map.empty Nothing) Map.empty)

    describe "Upvalue Ord instance" $ do
        it "should compare Upvalues correctly" $
            Upvalue (Variable VarInteger (VNumber 42)) 0 Nothing `shouldSatisfy` (< Upvalue (Variable VarInteger (VNumber 43)) 0 Nothing)

    describe "Function Ord instance" $ do
        it "should compare Functions correctly" $
            Function 0 0 (Chunk [] [] []) Nothing `shouldSatisfy` (< Function 1 0 (Chunk [] [] []) Nothing)

    describe "Object Eq instance" $ do
        it "should return True for equal OString" $
            OString "Hello" `shouldBe` OString "Hello"

        it "should return False for different OString" $
            OString "Hello" `shouldNotBe` OString "World"

        it "should return True for equal OFunction" $
            OFunction (Function 0 0 (Chunk [] [] []) Nothing) `shouldBe` OFunction (Function 0 0 (Chunk [] [] []) Nothing)

        it "should return False for different OFunction" $
            OFunction (Function 0 0 (Chunk [] [] []) Nothing) `shouldNotBe` OFunction (Function 1 0 (Chunk [] [] []) Nothing)

        it "should return True for equal OClosure" $
            OClosure (Closure (Function 0 0 (Chunk [] [] []) Nothing) []) `shouldBe` OClosure (Closure (Function 0 0 (Chunk [] [] []) Nothing) [])

        it "should return False for different OClosure" $
            OClosure (Closure (Function 0 0 (Chunk [] [] []) Nothing) []) `shouldNotBe` OClosure (Closure (Function 1 0 (Chunk [] [] []) Nothing) [])

        it "should return True for equal OUpvalue" $
            OUpvalue (Upvalue (Variable VarInteger (VNumber 42)) 0 Nothing) `shouldBe` OUpvalue (Upvalue (Variable VarInteger (VNumber 42)) 0 Nothing)

        it "should return False for different OUpvalue" $
            OUpvalue (Upvalue (Variable VarInteger (VNumber 42)) 0 Nothing) `shouldNotBe` OUpvalue (Upvalue (Variable VarBool (VBool True)) 0 Nothing)

        it "should return True for equal OClass" $
            OClass (Class "Test" Map.empty Nothing) `shouldBe` OClass (Class "Test" Map.empty Nothing)

        it "should return False for different OClass" $
            OClass (Class "Test" Map.empty Nothing) `shouldNotBe` OClass (Class "Test2" Map.empty Nothing)

        it "should return True for equal OInstance" $
            OInstance (Instance (Class "Test" Map.empty Nothing) Map.empty) `shouldBe` OInstance (Instance (Class "Test" Map.empty Nothing) Map.empty)

        it "should return False for different OInstance" $
            OInstance (Instance (Class "Test" Map.empty Nothing) Map.empty) `shouldNotBe` OInstance (Instance (Class "Test2" Map.empty Nothing) Map.empty)

        it "should return True for equal OBoundMethod" $
            OBoundMethod (BoundMethod (Variable VarInteger (VNumber 42)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) [])) `shouldBe` OBoundMethod (BoundMethod (Variable VarInteger (VNumber 42)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) []))

        it "should return False for different OBoundMethod" $
            OBoundMethod (BoundMethod (Variable VarInteger (VNumber 42)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) [])) `shouldNotBe` OBoundMethod (BoundMethod (Variable VarBool (VBool True)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) []))

        it "should return True for equal OList" $
            OList [Variable VarInteger (VNumber 42)] `shouldBe` OList [Variable VarInteger (VNumber 42)]

        it "should return False for different OList" $
            OList [Variable VarInteger (VNumber 42)] `shouldNotBe` OList [Variable VarInteger (VNumber 43)]

        it "should return True for equal OMap" $
            OMap (Map.singleton (Variable VarString (VObj $ OString "key")) (Variable VarInteger (VNumber 42))) `shouldBe` OMap (Map.singleton (Variable VarString (VObj $ OString "key")) (Variable VarInteger (VNumber 42)))

        it "should return False for different OMap" $
            OMap (Map.singleton (Variable VarString (VObj $ OString "key")) (Variable VarInteger (VNumber 42))) `shouldNotBe` OMap (Map.singleton (Variable VarString (VObj $ OString "key")) (Variable VarInteger (VNumber 43)))

        it "should return False for different Object types" $
            OString "Hello" `shouldNotBe` OFunction (Function 0 0 (Chunk [] [] []) Nothing)

    describe "Chunk Eq instance" $ do
        it "should return True for equal Chunks" $
            Chunk [1, 2, 3] [1, 2, 3] [Variable VarInteger (VNumber 42)] `shouldBe` Chunk [1, 2, 3] [1, 2, 3] [Variable VarInteger (VNumber 42)]

        it "should return False for Chunks with different codes" $
            Chunk [1, 2, 3] [1, 2, 3] [Variable VarInteger (VNumber 42)] `shouldNotBe` Chunk [4, 5, 6] [1, 2, 3] [Variable VarInteger (VNumber 42)]

        it "should return False for Chunks with different lines" $
            Chunk [1, 2, 3] [1, 2, 3] [Variable VarInteger (VNumber 42)] `shouldNotBe` Chunk [1, 2, 3] [4, 5, 6] [Variable VarInteger (VNumber 42)]

        it "should return False for Chunks with different constants" $
            Chunk [1, 2, 3] [1, 2, 3] [Variable VarInteger (VNumber 42)] `shouldNotBe` Chunk [1, 2, 3] [1, 2, 3] [Variable VarInteger (VNumber 43)]

    describe "Value Eq instance" $ do
        it "should return True for equal VNil" $
            VNil `shouldBe` VNil

        it "should return True for equal VBool" $
            VBool True `shouldBe` VBool True

        it "should return False for different VBool" $
            VBool True `shouldNotBe` VBool False

        it "should return True for equal VNumber" $
            VNumber 42.0 `shouldBe` VNumber 42.0

        it "should return False for different VNumber" $
            VNumber 42.0 `shouldNotBe` VNumber 43.0

        it "should return True for equal VObj" $
            VObj (OString "Hello") `shouldBe` VObj (OString "Hello")

        it "should return False for different VObj" $
            VObj (OString "Hello") `shouldNotBe` VObj (OString "World")

    describe "Value Ord instance" $ do
        it "should compare VNil correctly" $
            VNil `shouldSatisfy` (< VBool True)

        it "should compare VBool correctly" $
            VBool False `shouldSatisfy` (< VBool True)

        it "should compare VNumber correctly" $
            VNumber 42.0 `shouldSatisfy` (< VNumber 43.0)

        it "should compare VObj correctly" $
            VObj (OString "Hello") `shouldSatisfy` (< VObj (OString "World"))


    describe "BoundMethod" $ do
        it "should show BoundMethod correctly" $
            show (BoundMethod (Variable VarInteger (VNumber 42)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) [])) `shouldBe` "BoundMethod {boundReceiver = 42, boundMethod = Closure {closureFunction = Function {functionArity = 0, functionUpvalueCount = 0, functionChunk = Chunk {chunkCode = [], chunkLines = [], chunkConstants = []}, functionName = Nothing}, closureUpvalues = []}}"

        it "should compare BoundMethods correctly" $
            BoundMethod (Variable VarInteger (VNumber 42)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) []) `shouldBe` BoundMethod (Variable VarInteger (VNumber 42)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) [])

    describe "Class" $ do
        it "should show Class correctly" $
            show (Class "Test" Map.empty Nothing) `shouldBe` "Class {className = \"Test\", classMethods = fromList [], classSuperclass = Nothing}"

        it "should compare Classes correctly" $
            Class "Test" Map.empty Nothing `shouldBe` Class "Test" Map.empty Nothing

    describe "Closure" $ do
        it "should show Closure correctly" $
            show (Closure (Function 0 0 (Chunk [] [] []) Nothing) []) `shouldBe` "Closure {closureFunction = Function {functionArity = 0, functionUpvalueCount = 0, functionChunk = Chunk {chunkCode = [], chunkLines = [], chunkConstants = []}, functionName = Nothing}, closureUpvalues = []}"

        it "should compare Closures correctly" $
            Closure (Function 0 0 (Chunk [] [] []) Nothing) [] `shouldBe` Closure (Function 0 0 (Chunk [] [] []) Nothing) []

    describe "Instance" $ do
        it "should show Instance correctly" $
            show (Instance (Class "Test" Map.empty Nothing) Map.empty) `shouldBe` "Instance {instanceClass = Class {className = \"Test\", classMethods = fromList [], classSuperclass = Nothing}, instanceFields = fromList []}"

        it "should compare Instances correctly" $
            Instance (Class "Test" Map.empty Nothing) Map.empty `shouldBe` Instance (Class "Test" Map.empty Nothing) Map.empty

    describe "Upvalue" $ do
        it "should show Upvalue correctly" $
            show (Upvalue (Variable VarInteger (VNumber 42)) 0 Nothing) `shouldBe` "Upvalue {upvalueClosed = 42, upvalueLocation = 0, upvalueNext = Nothing}"

        it "should compare Upvalues correctly" $
            Upvalue (Variable VarInteger (VNumber 42)) 0 Nothing `shouldBe` Upvalue (Variable VarInteger (VNumber 42)) 0 Nothing

    describe "Function" $ do
        it "should show Function correctly" $
            show (Function 0 0 (Chunk [] [] []) Nothing) `shouldBe` "Function {functionArity = 0, functionUpvalueCount = 0, functionChunk = Chunk {chunkCode = [], chunkLines = [], chunkConstants = []}, functionName = Nothing}"

        it "should compare Functions correctly" $
            Function 0 0 (Chunk [] [] []) Nothing `shouldBe` Function 0 0 (Chunk [] [] []) Nothing

        it "should compare BoundMethods with different boundReceivers correctly" $
            BoundMethod (Variable VarInteger (VNumber 42)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) []) `shouldNotBe` BoundMethod (Variable VarInteger (VNumber 43)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) [])

        it "should compare BoundMethods with different boundMethods correctly" $
            BoundMethod (Variable VarInteger (VNumber 42)) (Closure (Function 0 0 (Chunk [] [] []) Nothing) []) `shouldNotBe` BoundMethod (Variable VarInteger (VNumber 42)) (Closure (Function 1 0 (Chunk [] [] []) Nothing) [])

        it "should compare Classes with different classNames correctly" $
            Class "Test1" Map.empty Nothing `shouldNotBe` Class "Test2" Map.empty Nothing

        it "should compare Classes with different classMethods correctly" $
            Class "Test" (Map.singleton "method" (Variable VarInteger (VNumber 42))) Nothing `shouldNotBe` Class "Test" Map.empty Nothing

        it "should compare Classes with different classSuperclasses correctly" $
            Class "Test" Map.empty (Just (Class "Super" Map.empty Nothing)) `shouldNotBe` Class "Test" Map.empty Nothing

        it "should show VBool True correctly" $
            show (VBool True) `shouldBe` "true"

        it "should show VBool False correctly" $
            show (VBool False) `shouldBe` "false"
