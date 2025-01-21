{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- Base
-}

module Types.Base (
    Variable(..),
    Value(..),
    VarType(..),
    Object(..),
    Function(..),
    Chunk(..),
    Class(..),
    Instance(..),
    Upvalue(..),
    BoundMethod(..),
    Closure(..),
    nextEscape
) where

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Binary ( Word8, Binary(get, put), getWord8, putWord8 )
import Control.Monad (replicateM)

data VarType
    = VarInteger
    | VarBool
    | VarString
    | VarDouble
    | VarAny
    | VarClass
    | VarTuple [VarType]
    | VarMap VarType VarType Bool
    deriving (Eq, Ord)

instance Show VarType where
    show VarInteger  = "Integer"
    show VarBool     = "Bool"
    show VarString   = "String"
    show VarDouble   = "Double"
    show VarAny      = "Any Type"
    show VarClass    = "Class"
    show (VarTuple l) = "Tuple of [" ++ concatMap (\x -> show x ++ ", ") l ++ "]"
    show (VarMap k e True) = "SafeMap of [" ++ show k ++ ", " ++ show e ++ "]"
    show (VarMap k e False) = "Map of [" ++ show k ++ ", " ++ show e ++ "]"

data Variable = Variable
    { varType :: VarType
    , varValue :: Value
    } deriving (Ord)

instance Show Variable where
    show v = show $ varValue v

instance Eq Variable where
    l == r = (varValue l == varValue r) && (varType l == varType r)

data Value
    = VNil
    | VBool Bool
    | VNumber Double
    | VObj Object
    deriving (Eq, Ord)

instance Show Value where
    show VNil = "nil"
    show (VBool b) = if b then "true" else "false"
    show (VNumber n) =
        if n == fromInteger (round n)
        then show (round n :: Integer)
        else show n
    show (VObj obj) = show obj

data Object
    = OBoundMethod BoundMethod
    | OClass Class
    | OClosure Closure
    | OFunction Function
    | OInstance Instance
    | OString T.Text
    | OUpvalue Upvalue
    | OList [Variable]
    | OMap (Map.Map Variable Variable)
    deriving (Ord)

instance Show Object where
    show (OString text) = T.unpack $ interpretEscapes text
    show (OFunction f) = "<fn> " ++ show f
    show (OClosure _) = "<closure>"
    show (OUpvalue u) = "<upvalue> " ++ show u
    show (OClass c) = "<class> " ++ show c
    show (OInstance i) = "<instance> " ++ show i
    show (OBoundMethod m) = "<bound method> " ++ show m
    show (OList l) = "<list> " ++ show l
    show (OMap k) = "<map> " ++ show k

interpretEscapes :: T.Text -> T.Text
interpretEscapes = T.pack . nextEscape . T.unpack

nextEscape :: [Char] -> [Char]
nextEscape [] = []
nextEscape ('\\':'n':xs) =   '\n' : nextEscape xs
nextEscape ('\\':'t':xs) =   '\t' : nextEscape xs
nextEscape ('\\':'r':xs) =   '\r' : nextEscape xs
nextEscape ('\\':'\\':xs) =   '\\': nextEscape xs
nextEscape ('\\':'"':xs) =    '"' : nextEscape xs
nextEscape ('\\':'\'':xs) =  '\'' : nextEscape xs
nextEscape ('\\':x:xs) = '\\' : x : nextEscape xs
nextEscape (x:xs) =             x : nextEscape xs

instance Eq Object where
    (OString a) == (OString b) = a == b
    (OFunction a) == (OFunction b) = a == b
    (OClosure a) == (OClosure b) = a == b
    (OUpvalue a) == (OUpvalue b) = a == b
    (OClass a) == (OClass b) = a == b
    (OInstance a) == (OInstance b) = a == b
    (OBoundMethod a) == (OBoundMethod b) = a == b
    (OList l) == (OList l') = l == l'
    (OMap k) == (OMap k') = k == k'
    _ == _ = False

data BoundMethod = BoundMethod
    { boundReceiver :: Variable
    , boundMethod :: Closure
    } deriving (Show, Eq, Ord)

data Class = Class
    { className :: T.Text
    , classMethods :: Map.Map T.Text Variable
    , classSuperclass :: Maybe Class
    } deriving (Show, Eq, Ord)

data Closure = Closure
    { closureFunction :: Function
    , closureUpvalues :: [Upvalue]
    } deriving (Show, Eq, Ord)

data Instance = Instance
    { instanceClass :: Class
    , instanceFields :: Map.Map T.Text Variable
    } deriving (Show, Eq, Ord)

data Upvalue = Upvalue
    { upvalueClosed :: Variable
    , upvalueLocation :: Int
    , upvalueNext :: Maybe Upvalue
    } deriving (Show, Eq, Ord)

data Function = Function
    { functionArity :: Int
    , functionUpvalueCount :: Int
    , functionChunk :: Chunk
    , functionName :: Maybe T.Text
    } deriving (Show, Eq, Ord)

data Chunk = Chunk
    { chunkCode :: [Word8]
    , chunkLines :: [Int]
    , chunkConstants :: [Variable]
    } deriving (Show, Eq, Ord)

instance Binary BoundMethod where
    put (BoundMethod receiver method) = do
        put receiver
        put method
    get = BoundMethod <$> get <*> get

instance Binary Class where
    put (Class name methods superclass) = do
        put name
        put methods
        put superclass
    get = Class <$> get <*> get <*> get

instance Binary Closure where
    put (Closure func upvalues) = do
        put func
        put upvalues
    get = Closure <$> get <*> get

instance Binary Instance where
    put (Instance cls fields) = do
        put cls
        put fields
    get = Instance <$> get <*> get

instance Binary Upvalue where
    put (Upvalue closed location next) = do
        put closed
        put location
        put next
    get = Upvalue <$> get <*> get <*> get

instance Binary Function where
    put (Function arity upvalueCount chunk name) = do
        put arity
        put upvalueCount
        put chunk
        put name
    get = Function <$> get <*> get <*> get <*> get

instance Binary Chunk where
    put (Chunk code l constants) = do
        put code
        put l
        put constants
    get = Chunk <$> get <*> get <*> get

instance Binary Variable where
    put (Variable vType vValue) = do
        put vType
        put vValue

    get = do
        Variable <$> get <*> get

instance Binary Value where
    put VNil = putWord8 0
    put (VBool b) = putWord8 1 >> put b
    put (VNumber n) = putWord8 2 >> put n
    put (VObj obj) = putWord8 3 >> put obj

    get = do
        tag <- getWord8
        case tag of
            0 -> pure VNil
            1 -> VBool <$> get
            2 -> VNumber <$> get
            3 -> VObj <$> get
            _ -> fail "Unknown Value tag"

instance Binary Object where
    put (OString text) = putWord8 0 >> put text
    put (OList values) = putWord8 1 >> put values
    put (OMap values) = putWord8 2 >> put values
    put (OBoundMethod boundMet) = putWord8 3 >> put boundMet
    put (OClass cls) = putWord8 4 >> put cls
    put (OClosure closure) = putWord8 5 >> put closure
    put (OFunction func) = putWord8 6 >> put func
    put (OInstance inst) = putWord8 7 >> put inst
    put (OUpvalue upvalue) = putWord8 9 >> put upvalue

    get = do
        tag <- getWord8
        case tag of
            0 -> OString <$> get
            1 -> OList <$> get
            2 -> OMap <$> get
            3 -> OBoundMethod <$> get
            4 -> OClass <$> get
            5 -> OClosure <$> get
            6 -> OFunction <$> get
            7 -> OInstance <$> get
            9 -> OUpvalue <$> get
            _ -> fail "Unknown Object tag"

instance Binary VarType where
    put VarInteger = putWord8 0
    put VarBool = putWord8 1
    put VarString = putWord8 2
    put VarDouble = putWord8 3
    put VarAny = putWord8 4
    put VarClass = putWord8 5
    put (VarTuple l) = do
        putWord8 6
        putWord8 (fromIntegral (length l))
        mapM_ put l
    put (VarMap k e s) = do
        putWord8 7
        put k
        put e
        put s
    get = do
        tag <- getWord8
        case tag of
            0 -> pure VarInteger
            1 -> pure VarBool
            2 -> pure VarString
            3 -> pure VarDouble
            4 -> pure VarAny
            5 -> pure VarClass
            6 -> do
                len <- getWord8
                elems <- replicateM (fromIntegral len) get
                pure (VarTuple elems)
            7 -> do
                k <- get
                e <- get
                VarMap k e <$> get
            _ -> fail "Unknown VarType tag"
