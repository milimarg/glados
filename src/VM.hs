{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- VM
-}

module VM (
    VMEnv(..),
    InterpretResult(..),
    CallFrame(..),
    initVM,
    interpret,
    push,
    pop,
    peek,
    currentFrame,
    readByte,
    readShort,
    readConstant,
    readString,
    binaryOp,
    getBinaryOperands,
    call,
    callValue,
    callObject,
    callBoundMethod,
    callClass,
    callClosure,
    bindMethod,
    captureUpvalue,
    closeUpvalues,
    closeUpvalue,
    isFalsey,
    run,
    runtimeError,
    decodeShort,
    advanceIp,
    updateInstructionPointer,
    addError,
    runFunction,
    validateAndCall,
    reportArityError,
    pushCallFrame,
    makeCallFrame,
    pushClassInstance,
    handleClassInitializer,
    checkClassInitArgCount,
    invokeFromClass,
    findMethodInClassHierarchy,
    callMethod,
    invoke,
    invokeInstance,
    handleInvokeResult,
    handleBindMethod,
    executeOp,
    executeConstant,
    executeNil,
    executeTrue,
    executeFalse,
    executePop,
    executeGetLocal,
    peekDistanceReversed,
    executeSetLocal,
    adjustSlot,
    updateLocalValue,
    executeGetGlobal,
    word8ToVarType,
    getVarType,
    executeDefineGlobal,
    executeSetGlobal,
    handleSetGlobal,
    executeGetUpvalue,
    executeSetUpvalue,
    updateUpvalue,
    executeGetProperty,
    handleGetProperty,
    executeSetProperty,
    handleSetProperty,
    prepareGetListMethod,
    executeGetMethod,
    checkSafeMap,
    checkValueList,
    executeSetMethod,
    handleSetListMethod,
    executeSetListGlobal,
    handleSetListGlobal,
    handleSetGLobalMapEnv,
    handleSetGLobalListEnv,
    handleSetGlobalEnv,
    executeSetListLocal,
    executeSetListUpValue,
    executeGetSuper,
    handleGetSuper,
    handleSuperMethod,
    executeEqual,
    executeGreater,
    executeLess,
    executeAdd,
    handleAdd,
    pushAddResult,
    executeSubtract,
    executeMultiply,
    executeDivide,
    executeModulo,
    myModulo,
    executeNot,
    executeNegate,
    executePrint,
    executePrintLn,
    executeSetRecursionLimit,
    executeJump,
    executeJumpIfFalse,
    executeLoop,
    executeCall,
    executeInvoke,
    executeSuperInvoke,
    executeClosure,
    handleClosureCreation,
    extractUpvalue,
    executeCloseUpvalue,
    executeReturn,
    modifyEnvForReturn,
    handleReturnFromFrame,
    updateEnvForReturn,
    executeClass,
    executeInherit,
    handleInherit,
    executeMethod,
    handleMethodDefinition,
    updateClassInGlobals,
    peekClass,
    updateFrameIp,
    handleBinaryOperation,
    handleBinaryOperationWithFPECheck,
    handleComparison,
    checkValueWithTuple,
    checkValueWithType,
    boolToDouble,
    chooseTypeAdd
) where

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Word (Word8, Word16)
import Control.Monad.State (modify, MonadState(put, get), StateT(runStateT), gets)
import Data.Bits (Bits(..))
import Types.Base (
    Chunk(..),
    Function (..),
    Variable(..),
    VarType(..),
    Value(..),
    Object(..),
    Class(..),
    Instance(..),
    Upvalue(..),
    Closure(..),
    BoundMethod(..))
import Control.Monad (when, void, unless, replicateM)
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Data.Time.Clock (UTCTime)
import Control.Monad.Identity (Identity(..))
import Chunk (OpCode (..))
import Utils (safeIndex)

data InterpretResult
    = InterpretOk
    | InterpretCompileError
    | InterpretRuntimeError
    deriving (Show, Eq)

data VMEnv = VMEnv
    { envStack :: [Variable]
    , envFrames :: [CallFrame]
    , envGlobals :: Map.Map T.Text Variable
    , envOpenUpvalues :: [Object]
    , envInitString :: T.Text
    , envStdoutBuffer :: [T.Text]
    , envStderrBuffer :: [T.Text]
    , envDebugMode :: Bool
    , envInterpretResult :: InterpretResult
    , envStartTime :: UTCTime
    , recursionLimit :: Int
    } deriving (Show, Eq)

data CallFrame = CallFrame
    { frameClosure :: Closure
    , frameIp :: Int
    , frameSlots :: Int
    } deriving (Show, Eq)

type VMM = ExceptT T.Text (StateT VMEnv Identity)

initVM :: Bool -> UTCTime -> VMEnv
initVM debug startTime = VMEnv
    { envStack = [], envFrames = [], envOpenUpvalues = []
    , envGlobals = Map.empty
    , envInitString = T.pack "init"
    , envStdoutBuffer = [], envStderrBuffer = []
    , envDebugMode = debug
    , envInterpretResult = InterpretOk
    , envStartTime = startTime
    , recursionLimit = 1000000}

interpret :: Function -> VMEnv -> (InterpretResult, VMEnv)
interpret entryPoint env =
    case runIdentity $ runStateT (runExceptT (runFunction entryPoint)) env of
        (Left err, env') -> (envInterpretResult env', addError env' err)
        (Right _, env')  -> (envInterpretResult env', env')

addError :: VMEnv -> T.Text -> VMEnv
addError env err = env { envStderrBuffer = envStderrBuffer env ++ [err] }

runFunction :: Function -> VMM ()
runFunction f = do
    let closure = Closure f []
        var = Variable VarAny (VObj $ OClosure closure)
    push var
    _ <- call (OClosure closure) 0
    run

runtimeError :: String -> VMM a
runtimeError msg =
    modify (\env -> env { envInterpretResult = InterpretRuntimeError }) >>
    throwError (T.pack msg)

push :: Variable -> VMM ()
push value =
    modify $ \env ->
        env
            { envStack = value : envStack env
            }

pop :: VMM Variable
pop = do
    env <- get
    case envStack env of
        [] -> runtimeError "Stack underflow."
        (x:xs) ->
            put env
                { envStack = xs
                } >>
            return x

peek :: Int -> VMM Variable
peek distance = do
    env <- get
    let stack = envStack env
    case safeIndex stack distance of
        Just value -> return value
        Nothing -> runtimeError "Index out of bounds"

currentFrame :: VMM CallFrame
currentFrame = do
    env <- get
    case envFrames env of
        [] -> runtimeError "No frames"
        (frame:_) -> return frame

readByte :: VMM Word8
readByte = do
    frame <- currentFrame
    let ip = frameIp frame
    let function = closureFunction (frameClosure frame)
    let code = chunkCode (functionChunk function)
    case safeIndex code ip of
        Just byte ->
            updateInstructionPointer (ip + 1) frame >>
            return byte
        Nothing -> runtimeError "Invalid instruction pointer (out of bounds)"

updateInstructionPointer :: Int -> CallFrame -> VMM ()
updateInstructionPointer newIp frame = modify $ \env ->
    env { envFrames = frame { frameIp = newIp } : tail (envFrames env) }

readShort :: VMM Word16
readShort = do
    frame <- currentFrame
    let ip = frameIp frame
    short <- decodeShort ip
    advanceIp 2
    return short

decodeShort :: Int -> VMM Word16
decodeShort ip = do
    code <- gets (chunkCode . functionChunk . closureFunction
        . frameClosure . head . envFrames)
    byte1 <- case safeIndex code ip of
        Just b -> return b
        Nothing -> runtimeError "Invalid instruction pointer (out of bounds)"
    byte2 <- case safeIndex code (ip + 1) of
        Just b -> return b
        Nothing -> runtimeError "Invalid instruction pointer (out of bounds)"
    return $ (fromIntegral byte1 `shiftL` 8) .|. fromIntegral byte2

advanceIp :: Int -> VMM ()
advanceIp offset = modify $ \env ->
    case envFrames env of
        (frame:frames) -> env { envFrames =
            frame { frameIp = frameIp frame + offset } : frames }
        [] -> env

readConstant :: VMM Variable
readConstant = do
    frame <- currentFrame
    let function = closureFunction (frameClosure frame)
    let chunk = functionChunk function
    let constants = chunkConstants chunk
    idx <- readByte
    case safeIndex constants (fromIntegral idx) of
        Just value -> return value
        Nothing -> runtimeError "Constant index out of bounds"

readString :: VMM T.Text
readString = do
    constant <- readConstant
    case varValue constant of
        VObj (OString text) -> return text
        _ -> runtimeError "Expected string constant"

getBinaryOpResultType :: VarType -> VarType -> VarType
getBinaryOpResultType VarDouble _ = VarDouble
getBinaryOpResultType _ VarDouble = VarDouble
getBinaryOpResultType VarInteger _ = VarInteger
getBinaryOpResultType _ VarInteger = VarInteger
getBinaryOpResultType VarAny _ = VarAny
getBinaryOpResultType _ VarAny = VarAny
getBinaryOpResultType _ _ = error "Invalid type for binary op."

getBinaryOpTypeResult :: VMM VarType
getBinaryOpTypeResult = do
    first <- pop
    second <- pop
    let resultType = getBinaryOpResultType (varType first) (varType second)
    return resultType

binaryOp :: (Double -> Double -> Double) -> VMM InterpretResult
binaryOp op = do
    (a, b) <- getBinaryOperands
    type_ <- getBinaryOpTypeResult
    let var = Variable type_ (VNumber (op a b))
    push var
    return InterpretOk

getBinaryOperands :: VMM (Double, Double)
getBinaryOperands = do
    b <- peek 0
    a <- peek 1
    case (varValue a, varValue b) of
        (VNumber n1, VNumber n2) -> return (n1, n2)
        (VBool bool, VNumber n) -> return (n, boolToDouble bool)
        (VNumber n, VBool bool) -> return (boolToDouble bool, n)
        _ -> runtimeError "Operands must be numbers."

call :: Object -> Int -> VMM Bool
call callee argCount = case callee of
    OClosure closure -> validateAndCall closure argCount
    _ -> runtimeError "Can only call functions and classes." >> return False

validateAndCall :: Closure -> Int -> VMM Bool
validateAndCall closure argCount =
    if functionArity (closureFunction closure) /= argCount
        then reportArityError closure argCount >> return False
        else pushCallFrame closure argCount >> return True

reportArityError :: Closure -> Int -> VMM ()
reportArityError closure argCount = runtimeError $
    "Expected " ++ show (functionArity (closureFunction closure)) ++
    " arguments but got " ++ show argCount

pushCallFrame :: Closure -> Int -> VMM ()
pushCallFrame closure argCount = do
    frame <- makeCallFrame closure argCount
    modify $ \e -> e { envFrames = frame : envFrames e }

makeCallFrame :: Closure -> Int -> VMM CallFrame
makeCallFrame closure argCount = do
    env <- get
    let stackSize = length (envStack env)
    return CallFrame
        { frameClosure = closure
        , frameIp = 0
        , frameSlots = stackSize - argCount - 1
        }

callValue :: Variable -> Int -> VMM Bool
callValue (Variable _ (VObj obj)) = callObject obj
callValue _ = const $ runtimeError "Can only call functions and classes."

callObject :: Object -> Int -> VMM Bool
callObject obj n = case obj of
    OBoundMethod bound -> callBoundMethod bound n
    OClass cls -> callClass cls n
    OClosure closure -> call (OClosure closure) n
    _ -> runtimeError "Can only call functions and classes."

callBoundMethod :: BoundMethod -> Int -> VMM Bool
callBoundMethod bound argCount = do
    _ <- pop
    push $ boundReceiver bound
    call (OClosure $ boundMethod bound) argCount

callClass :: Class -> Int -> VMM Bool
callClass cls argCount = do
    initStr <- gets envInitString
    let inst = Instance cls Map.empty
    pushClassInstance inst
    handleClassInitializer cls initStr argCount

pushClassInstance :: Instance -> VMM ()
pushClassInstance inst = do
    _ <- pop
    push stackElem
  where
    stackElem = Variable VarAny (VObj (OInstance inst))

handleClassInitializer :: Class -> T.Text -> Int -> VMM Bool
handleClassInitializer cls initStr argCount =
    case Map.lookup initStr (classMethods cls) of
        Just (Variable _ (VObj (OClosure initializer))) ->
            call (OClosure initializer) argCount
        Just _ -> runtimeError "Invalid initializer."
        Nothing -> checkClassInitArgCount argCount

checkClassInitArgCount :: Int -> VMM Bool
checkClassInitArgCount 0 = return True
checkClassInitArgCount n = runtimeError $
    "Expected 0 arguments but got " ++ show n

invokeFromClass :: Class -> T.Text -> Int -> VMM Bool
invokeFromClass cls name argCount = do
    result <- findMethodInClassHierarchy cls name
    case result of
        Nothing -> do
            _ <- runtimeError $ "Undefined property '" <> T.unpack name <> "'"
            return False
        Just method -> callMethod method argCount

findMethodInClassHierarchy :: Monad m => Class -> T.Text -> m (Maybe Variable)
findMethodInClassHierarchy cls name =
    case Map.lookup name (classMethods cls) of
        Just method -> return (Just method)
        Nothing -> case classSuperclass cls of
            Nothing -> return Nothing
            Just superclass -> findMethodInClassHierarchy superclass name

callMethod :: Variable -> Int -> VMM Bool
callMethod method argCount = case varValue method of
    VObj (OClosure closure) -> call (OClosure closure) argCount
    _ -> do
        _ <- runtimeError "Invalid method type."
        return False

invoke :: T.Text -> Int -> VMM Bool
invoke name argCount = do
    receiver <- peek argCount
    case varValue receiver of
        VObj (OInstance inst) -> invokeInstance inst name argCount
        _ -> runtimeError "Only instances have methods."

invokeInstance :: Instance -> T.Text -> Int -> VMM Bool
invokeInstance inst name argCount = do
    let cls = instanceClass inst
    result <- findMethodInClassHierarchy cls name
    handleInvokeResult result name argCount
        (Variable VarAny (VObj (OInstance inst)))

handleInvokeResult :: Maybe Variable -> T.Text -> Int -> Variable -> VMM Bool
handleInvokeResult (Just (Variable _ (VObj (OClosure clos))))
    _ argCount receiver =
    let bnd = BoundMethod receiver clos
        var = Variable VarAny (VObj $ OBoundMethod bnd)
    in push var >> callClosure clos argCount
handleInvokeResult (Just _) _ _ _ = runtimeError "Invalid method type."
handleInvokeResult Nothing name _ _ =
    runtimeError $ "Undefined property '" <> T.unpack name <> "'."

callClosure :: Closure -> Int -> VMM Bool
callClosure closure argCount =
    let function = closureFunction closure
    in if argCount /= functionArity function
        then runtimeError $ "Expected " ++ show (functionArity function)
            ++ " arguments but got " ++ show argCount ++ "."
        else do
            frame <- makeCallFrame closure argCount
            modify $ \env -> env { envFrames = frame : envFrames env }
            return True

bindMethod :: Class -> T.Text -> VMM Bool
bindMethod cls name = case Map.lookup name (classMethods cls) of
    Nothing -> return False
    Just method -> do
        receiver <- peek 0
        case varValue method of
            VObj (OClosure closure) -> handleBindMethod receiver closure
            _ -> return False

handleBindMethod :: Variable -> Closure -> VMM Bool
handleBindMethod receiver closure =
    let bound = BoundMethod receiver closure
        var = Variable VarAny (VObj $ OBoundMethod bound)
    in void pop >> push var
    >> return True

captureUpvalue :: Int -> VMM Object
captureUpvalue location = do
    env <- get
    let stack = envStack env
    case safeIndex stack location of
        Just value ->
            let upvalue = Upvalue value location Nothing
            in modify (\e ->
                e { envOpenUpvalues = OUpvalue upvalue : envOpenUpvalues e })
            >> return (OUpvalue upvalue)
        Nothing -> runtimeError "Stack index out of bounds"

closeUpvalues :: Int -> VMM ()
closeUpvalues end = do
    env <- get
    let (toClose, remaining) = span isHigherLocation (envOpenUpvalues env)
    mapM_ closeUpvalue toClose
    modify $ \e -> e { envOpenUpvalues = remaining }
  where
    isHigherLocation (OUpvalue upvalue) = upvalueLocation upvalue >= end
    isHigherLocation _ = False

closeUpvalue :: Object -> VMM ()
closeUpvalue (OUpvalue upvalue) = do
    env <- get
    let stack = envStack env
    case safeIndex stack (upvalueLocation upvalue) of
        Just value ->
            let closed = Upvalue value (upvalueLocation upvalue) Nothing
            in modify $ \e ->
                e { envOpenUpvalues = OUpvalue closed : envOpenUpvalues e }
        Nothing -> runtimeError "Stack index out of bounds"
closeUpvalue _ = return ()

isFalsey :: Variable -> Bool
isFalsey (Variable _ VNil) = True
isFalsey (Variable _ (VBool False)) = True
isFalsey _ = False

run :: VMM ()
run = do
    instruction <- readByte
    let op = toEnum (fromIntegral instruction)
    case op of
        OpReturn -> executeReturn
        o -> executeOp o >> run

executeOp :: OpCode -> VMM ()
executeOp OpConstant = executeConstant
executeOp OpNil = executeNil
executeOp OpTrue = executeTrue
executeOp OpFalse = executeFalse
executeOp OpPop = executePop
executeOp OpGetLocal = executeGetLocal
executeOp OpSetLocal = executeSetLocal
executeOp OpGetGlobal = executeGetGlobal
executeOp OpDefineGlobal = executeDefineGlobal
executeOp OpSetGlobal = executeSetGlobal
executeOp OpGetUpvalue = executeGetUpvalue
executeOp OpSetUpvalue = executeSetUpvalue
executeOp OpGetProperty = executeGetProperty
executeOp OpSetProperty = executeSetProperty
executeOp OpGetSuper = executeGetSuper
executeOp OpEqual = executeEqual
executeOp OpGreater = executeGreater
executeOp OpLess = executeLess
executeOp OpAdd = executeAdd
executeOp OpSubtract = executeSubtract
executeOp OpMultiply = executeMultiply
executeOp OpDivide = executeDivide
executeOp OpModulo = executeModulo
executeOp OpNot = executeNot
executeOp OpNegate = executeNegate
executeOp OpPrint = executePrint
executeOp OpPrintLn = executePrintLn
executeOp OpSetRecursionLimit = executeSetRecursionLimit
executeOp OpJump = executeJump
executeOp OpJumpIfFalse = executeJumpIfFalse
executeOp OpLoop = executeLoop
executeOp OpCall = executeCall
executeOp OpInvoke = executeInvoke
executeOp OpSuperInvoke = executeSuperInvoke
executeOp OpClosure = executeClosure
executeOp OpCloseUpvalue = executeCloseUpvalue
executeOp OpClass = executeClass
executeOp OpInherit = executeInherit
executeOp OpMethod = executeMethod
executeOp OpGetAtIndex = executeGetMethod
executeOp OpSetAtIndex = executeSetMethod
executeOp _ = return ()

executeConstant :: VMM ()
executeConstant = do
    constant <- readConstant
    push constant

executeNil :: VMM ()
executeNil = push (Variable VarAny VNil)

executeTrue :: VMM ()
executeTrue = push (Variable VarBool (VBool True))

executeFalse :: VMM ()
executeFalse = push (Variable VarBool (VBool False))

executePop :: VMM ()
executePop = void pop

executeGetLocal :: VMM ()
executeGetLocal = do
    slot <- readByte
    frame <- currentFrame
    let slot' = slot + fromIntegral (frameSlots frame)
    value <- peekDistanceReversed $ fromIntegral slot'
    push value

peekDistanceReversed :: Int -> VMM Variable
peekDistanceReversed distance = do
    env <- get
    case drop distance (reverse $ envStack env) of
        [] -> runtimeError "Stack underflow."
        (x:_) -> return x

checkValueWithTuple :: VarType -> Value -> VarType -> Maybe Variable
checkValueWithTuple (VarTuple t1) values (VarTuple t2) =
    if t2 == t1
        then Just (Variable (VarTuple t2) values)
        else Nothing
checkValueWithTuple (VarTuple _) values (VarMap k e s) =
    case values of
        VObj (OList []) -> Just (Variable (VarMap k e s) values)
        _ -> Nothing
checkValueWithTuple (VarTuple t1) values t2 =
    if all (== t2) t1
        then Just (Variable t2 values)
        else Nothing
checkValueWithTuple _ _ _ = Nothing

checkValueWithType :: Variable -> VarType -> Maybe Variable
checkValueWithType var VarAny = Just (Variable VarAny (varValue var))
checkValueWithType (Variable (VarTuple t1) values) t2 =
    checkValueWithTuple (VarTuple t1) values t2
checkValueWithType (Variable t1 values) t2 =
    if t1 == t2
        then Just (Variable t2 values)
        else Nothing

executeSetLocal :: VMM ()
executeSetLocal = do
    slot <- readByte
    slot' <- adjustSlot slot
    value <- peek 0
    env <- get
    updatedStack <- updateLocalValue env slot' value
    modify (\e -> e { envStack = updatedStack })

adjustSlot :: Word8 -> VMM Int
adjustSlot slot = do
    frame <- currentFrame
    return $ fromIntegral slot + frameSlots frame

updateLocalValue :: VMEnv -> Int -> Variable -> VMM [Variable]
updateLocalValue env slot value =
    let stack = envStack env
        (after, before) = splitAt (length stack - slot - 1) stack
        previous = last before
    in case checkValueWithType value (varType previous) of
        Just v -> return $ after ++ [v] ++ tail before
        Nothing -> runtimeError "Bad assignation."

executeGetGlobal :: VMM ()
executeGetGlobal = do
    name <- readString
    env <- get
    case Map.lookup name (envGlobals env) of
        Nothing ->
            runtimeError $ "Undefined variable '" <> T.unpack name <> "'."
        Just value -> push value

word8ToVarType :: Word8 -> VMM VarType
word8ToVarType 0 = return VarInteger
word8ToVarType 1 = return VarBool
word8ToVarType 2 = return VarString
word8ToVarType 3 = return VarDouble
word8ToVarType 4 = return VarAny
word8ToVarType 5 = return VarClass
word8ToVarType 6 = do
    lengthByte <- readByte
    let len = fromIntegral lengthByte :: Int
    types <- replicateM len $ do
        nextByte <- readByte
        word8ToVarType nextByte
    return (VarTuple types)
word8ToVarType 7 = do
    k <- readByte
    key <- word8ToVarType k
    e <- readByte
    type_ <- word8ToVarType e
    s <- readByte
    let safe = s /= 0
    pure (VarMap key type_ safe)
word8ToVarType _ = runtimeError "Failed to get VarType." >> return VarAny

getVarType :: VMM VarType
getVarType = do
    byte <- readByte
    word8ToVarType byte

executeDefineGlobal :: VMM ()
executeDefineGlobal = do
    name <- readString
    type_ <- getVarType
    value <- peek 0
    case checkValueWithType value type_ of
        Just v ->
            modify (\env ->
                env { envGlobals = Map.insert name v (envGlobals env) })
            >> void pop
        Nothing -> runtimeError "Invalid Type for assignation."

executeSetGlobal :: VMM ()
executeSetGlobal = do
    name <- readString
    env <- get
    case Map.lookup name (envGlobals env) of
        Nothing ->
            runtimeError $ "Undefined variable '" <> T.unpack name <> "'."
        Just previous -> handleSetGlobal name previous

handleSetGlobal :: T.Text -> Variable -> VMM ()
handleSetGlobal name previous = do
    value <- peek 0
    case checkValueWithType value (varType previous) of
        Just v ->
            get >>
            modify (\e ->
                e { envGlobals = Map.insert name v (envGlobals e) })
        Nothing -> runtimeError "Invalid Type for assignation."

executeGetUpvalue :: VMM ()
executeGetUpvalue = do
    slot <- readByte
    frame <- currentFrame
    let upvalues = closureUpvalues (frameClosure frame)
    case safeIndex upvalues (fromIntegral slot) of
        Just upvalue ->
            case upvalue of
                Upvalue closed _ _ -> push closed
        Nothing -> runtimeError "Invalid upvalue slot"

executeSetUpvalue :: VMM ()
executeSetUpvalue = do
    slot <- readByte
    frame <- currentFrame
    value <- peek 0
    let upvalues = closureUpvalues (frameClosure frame)
    case safeIndex upvalues (fromIntegral slot) of
        Just upvalue -> updateUpvalue value upvalue
        Nothing -> runtimeError "Invalid upvalue slot"

updateUpvalue :: Variable -> Upvalue -> VMM ()
updateUpvalue value (Upvalue _ location next) =
    case checkValueWithType value (varType value) of
        Just v ->
            let newV = Upvalue v location next
            in modify $ \env -> env
            { envOpenUpvalues = OUpvalue newV : tail (envOpenUpvalues env) }
        Nothing -> runtimeError "Invalid Type for assignation."

executeGetProperty :: VMM ()
executeGetProperty = do
    name <- readString
    instance_ <- peek 0
    handleGetProperty name instance_

handleGetProperty :: T.Text -> Variable -> VMM ()
handleGetProperty name (Variable _ (VObj (OInstance inst))) =
    case Map.lookup name (instanceFields inst) of
        Just value -> void pop >> push value
        Nothing -> do
            success <- bindMethod (instanceClass inst) name
            unless success $ runtimeError $
                "Undefined property '" ++ T.unpack name ++ "'"
handleGetProperty _ _ = runtimeError "Only instances have properties."

executeSetProperty :: VMM ()
executeSetProperty = do
    name <- readString
    instance_ <- peek 1
    handleSetProperty name instance_

handleSetProperty :: T.Text -> Variable -> VMM ()
handleSetProperty name (Variable _ (VObj (OInstance inst))) = do
    value <- peek 0
    let newFields = Map.insert name value (instanceFields inst)
    let _ = inst { instanceFields = newFields }
    void pop >> void pop
    push value
handleSetProperty _ _ = runtimeError "Only instances have fields."

prepareGetListMethod :: OpCode -> VMM ()
prepareGetListMethod op = case op of
    OpGetGlobal -> executeGetGlobal
    OpGetLocal -> executeGetLocal
    OpGetUpvalue -> executeGetUpvalue
    _ -> runtimeError "Invalid get Op."

executeGetMethod :: VMM ()
executeGetMethod = do
    idx <- peek 0
    instruction <- readByte
    let op = toEnum (fromIntegral instruction)
    prepareGetListMethod op
    value <- pop
    checkValueList value idx

checkSafeMap :: Bool -> VMM ()
checkSafeMap True = push (Variable VarAny VNil)
checkSafeMap False = runtimeError "Invalid index for map."
checkValueList :: Variable -> Variable -> VMM ()
checkValueList (Variable _ (VObj (OList list))) idx =
    case idx of
        Variable VarInteger (VNumber position) ->
            case safeIndex list (round position) of
                Just value -> push value
                Nothing -> runtimeError "Index out of range."
        _ -> runtimeError "Can only indexing with Integer."
checkValueList (Variable (VarMap k _ s) (VObj (OMap m))) idx =
        if k == varType idx
            then case Map.lookup idx m of
                Just value -> push value
                Nothing -> checkSafeMap s
            else runtimeError "Invalid type for map get action."
checkValueList _ _ =
    runtimeError "Trying to index on something that is not a list."

executeSetMethod :: VMM ()
executeSetMethod = do
    value <- peek 1
    idx <- peek 0
    instruction <- readByte
    let op = toEnum (fromIntegral instruction)
    handleSetListMethod op value idx

handleSetListMethod :: OpCode -> Variable -> Variable -> VMM ()
handleSetListMethod OpSetGlobal value idx =
    executeSetListGlobal value idx
handleSetListMethod OpSetLocal value idx =
    executeSetListLocal value idx
handleSetListMethod OpSetUpvalue value idx =
    executeSetListUpValue value idx
handleSetListMethod _ _ _ = runtimeError "Bad set opCode."

executeSetListGlobal :: Variable -> Variable -> VMM ()
executeSetListGlobal value idx = do
    name <- readString
    env <- get
    case Map.lookup name (envGlobals env) of
        Nothing ->
            runtimeError $ "Undefined variable '" <> T.unpack name <> "'."
        Just previous -> handleSetListGlobal previous value idx name

handleSetListGlobal :: Variable -> Variable -> Variable -> T.Text -> VMM ()
handleSetListGlobal (Variable t1 (VObj (OList values))) val idx name =
    case idx of
        Variable VarInteger (VNumber position) ->
            let roundIndex = round position
            in case safeIndex values roundIndex of
                Just prev -> if varType prev == varType val
                    then handleSetGLobalListEnv t1 values val roundIndex name
                    else runtimeError "Invalid Type for assignation."
                Nothing -> runtimeError "List index out of bounds"
        _ -> runtimeError "Can only indexing with Integer."
handleSetListGlobal (Variable (VarMap k e s) (VObj (OMap m))) val idx name =
    if k == varType idx && e == varType val
        then handleSetGLobalMapEnv (VarMap k e s) m val idx name
        else runtimeError "Invalid type for map set action."
handleSetListGlobal _ _ _ _ = runtimeError "Bad set list action."

handleSetGLobalMapEnv :: VarType -> Map.Map Variable Variable -> Variable -> Variable -> T.Text -> VMM ()
handleSetGLobalMapEnv type_ m val idx name =
    let newMap = Map.insert idx val m
        var = Variable type_ (VObj (OMap newMap))
    in handleSetGlobalEnv name var

handleSetGLobalListEnv :: VarType -> [Variable] -> Variable -> Int -> T.Text -> VMM ()
handleSetGLobalListEnv t1 values val idx name =
    let (pre, post) = splitAt idx values
        newList = pre ++ [val] ++ if null post then [] else tail post
        var = Variable t1 (VObj (OList newList))
    in handleSetGlobalEnv name var

handleSetGlobalEnv :: T.Text -> Variable -> VMM ()
handleSetGlobalEnv name var =
    get >>
    modify (\e ->
        e { envGlobals = Map.insert name var (envGlobals e) })

executeSetListLocal :: Variable -> Variable -> VMM ()
executeSetListLocal _ _ = return ()

executeSetListUpValue :: Variable -> Variable -> VMM ()
executeSetListUpValue _ _ = return ()

executeGetSuper :: VMM ()
executeGetSuper = do
    name <- readString
    receiver <- pop
    handleGetSuper name receiver

handleGetSuper :: T.Text -> Variable -> VMM ()
handleGetSuper name (Variable _ (VObj (OInstance inst))) =
    case instanceClass inst of
        Class _ _ (Just super) -> handleSuperMethod name receiver super
        _ -> runtimeError "Object has no superclass."
    where receiver = Variable VarAny (VObj (OInstance inst))
handleGetSuper _ _ = runtimeError "Only instances have properties."

handleSuperMethod :: T.Text -> Variable -> Class -> VMM ()
handleSuperMethod name receiver super =
    case Map.lookup name (classMethods super) of
        Just (Variable _ (VObj (OClosure closure))) ->
            let bound = BoundMethod receiver closure
                var = Variable VarAny (VObj $ OBoundMethod bound)
            in push var
        _ -> runtimeError "Invalid method type."

executeEqual :: VMM ()
executeEqual = do
    b <- pop
    a <- pop
    let var = Variable VarBool (VBool $ a == b)
    push var

executeGreater :: VMM ()
executeGreater = handleComparison (>) "Operands must be numbers."

executeLess :: VMM ()
executeLess = handleComparison (<) "Operands must be numbers."

executeAdd :: VMM ()
executeAdd = do
    b <- peek 0
    a <- peek 1
    handleAdd a b

boolToDouble :: Bool -> Double
boolToDouble True = 1.0
boolToDouble False = 0.0

chooseTypeAdd :: VarType -> VarType -> Value -> Value -> VMM ()
chooseTypeAdd VarDouble _ (VNumber n1) (VNumber n2) =
        pushAddResult (Variable VarDouble (VNumber (n1 + n2)))
chooseTypeAdd _ VarDouble (VNumber n1) (VNumber n2) =
        pushAddResult (Variable VarDouble (VNumber (n1 + n2)))
chooseTypeAdd VarInteger _ (VNumber n1) (VNumber n2) =
        pushAddResult (Variable VarInteger (VNumber (n1 + n2)))
chooseTypeAdd _ VarInteger (VNumber n1) (VNumber n2) =
        pushAddResult (Variable VarInteger (VNumber (n1 + n2)))
chooseTypeAdd t1 t2 n1 n2 =
    case (n1, n2) of
        (VBool b, VNumber n) ->
            pushAddResult (Variable t2 (VNumber (n + boolToDouble b)))
        (VNumber n, VBool b) ->
            pushAddResult (Variable t1 (VNumber (n + boolToDouble b)))
        (VNumber v1, VNumber v2) ->
            pushAddResult (Variable t1 (VNumber (v1 + v2)))
        (_, _) -> runtimeError "Unsupported types for addition for number."

handleAdd :: Variable -> Variable -> VMM ()
handleAdd a b =
    case (varValue a, varValue b) of
        (VObj (OString s1),VObj (OString s2)) ->
            pushAddResult (Variable VarString (VObj (OString (s1 <> s2))))
        (n1, n2) -> chooseTypeAdd (varType a) (varType b) n1 n2

pushAddResult :: Variable -> VMM ()
pushAddResult result = do
    _ <- pop
    _ <- pop
    push result

executeSubtract :: VMM ()
executeSubtract = handleBinaryOperation (-) "Subtract error: "

executeMultiply :: VMM ()
executeMultiply = handleBinaryOperation (*) "Multiply error: "

executeDivide :: VMM ()
executeDivide = handleBinaryOperationWithFPECheck (/) "Divide error: "

executeModulo :: VMM()
executeModulo = handleBinaryOperationWithFPECheck myModulo "Modulo error: "

myModulo :: Double -> Double -> Double
myModulo x y = fromIntegral (floor x `mod` floor y :: Integer)

executeNot :: VMM ()
executeNot = do
    value <- pop
    let var = Variable VarBool (VBool $ isFalsey value)
    push var

executeNegate :: VMM ()
executeNegate = do
    value <- peek 0
    case value of
        (Variable type_ (VNumber n)) ->
            let var = Variable type_ (VNumber (-n))
            in void pop >> push var
        _ -> runtimeError "Operand must be a number."

executePrint :: VMM ()
executePrint = do
    value <- pop
    modify $ \env ->
        env { envStdoutBuffer = envStdoutBuffer env ++ [T.pack $ show value] }

executePrintLn :: VMM ()
executePrintLn = do
    value <- pop
    modify $ \env ->
        env { envStdoutBuffer =
            envStdoutBuffer env ++ [T.pack $ show value <> "\n"] }

executeSetRecursionLimit :: VMM ()
executeSetRecursionLimit = do
    value <- pop
    case varValue value of
        VNumber n -> modify $ \env -> env { recursionLimit = round n }
        _ -> runtimeError "Expected number for recursion limit argument."

executeJump :: VMM ()
executeJump = do
    off <- readShort
    fr <- currentFrame
    modify $ \env ->
        env { envFrames = updateFrameIp
            (frameIp fr + fromIntegral off) fr : tail (envFrames env) }

executeJumpIfFalse :: VMM ()
executeJumpIfFalse = do
    off <- readShort
    fr <- currentFrame
    condition <- peek 0
    when (isFalsey condition) $
        modify $ \env ->
            env { envFrames = updateFrameIp
                (frameIp fr + fromIntegral off) fr : tail (envFrames env) }

executeLoop :: VMM ()
executeLoop = do
    offset <- readShort
    frame <- currentFrame
    modify $ \env ->
        env { envFrames = updateFrameIp (frameIp frame - fromIntegral offset)
            frame : tail (envFrames env) }

executeCall :: VMM ()
executeCall = do
    env <- get
    if length (envFrames env) < recursionLimit env then do
        argCount <- readByte
        value <- peek (fromIntegral argCount)
        success <- callValue value (fromIntegral argCount)
        unless success $ runtimeError "Call failed."
    else
        runtimeError $ "Stack overflow: maximum stack depth of "
            ++ show (recursionLimit env) ++ " exceeded!"

executeInvoke :: VMM ()
executeInvoke = do
    method <- readString
    argCount <- readByte
    success <- invoke method (fromIntegral argCount)
    _ <- pop >> pop
    unless success $ runtimeError "Invoke failed."

executeSuperInvoke :: VMM ()
executeSuperInvoke = do
    method <- readString
    argCount <- readByte
    superclass <- pop
    case varValue superclass of
        VObj (OClass cls) -> do
            success <- invokeFromClass cls method (fromIntegral argCount)
            unless success $ runtimeError "Super invoke failed."
        _ -> runtimeError "Superclass must be a class."

executeClosure :: VMM ()
executeClosure = do
    function <- readConstant
    handleClosureCreation function

handleClosureCreation :: Variable -> VMM ()
handleClosureCreation (Variable _ (VObj (OFunction fn))) = do
    let count = functionUpvalueCount fn
    upvalues <- mapM captureUpvalue [0..count-1]
    ups <- mapM extractUpvalue upvalues
    let var = Variable VarAny (VObj $ OClosure $ Closure fn ups)
    push var
handleClosureCreation _ = runtimeError "Expected function constant."

extractUpvalue :: Object -> VMM Upvalue
extractUpvalue (OUpvalue up) = return up
extractUpvalue _ = runtimeError "Expected upvalue."

executeCloseUpvalue :: VMM ()
executeCloseUpvalue = do
    env <- get
    closeUpvalues (length (envStack env) - 1)
    void pop

executeReturn :: VMM ()
executeReturn = do
    result <- pop
    frame <- currentFrame
    closeUpvalues (frameSlots frame)
    modifyEnvForReturn result

modifyEnvForReturn :: Variable -> VMM ()
modifyEnvForReturn res = do
    env <- get
    case envFrames env of
        []      -> runtimeError "No frames to return from"
        [_]     -> void $ push res
        (_:fs)  -> handleReturnFromFrame res fs

handleReturnFromFrame :: Variable -> [CallFrame] -> VMM ()
handleReturnFromFrame res fs = do
    slots <- gets (frameSlots . head . envFrames)
    stack <- gets envStack
    let toDrop = length stack - slots
    updateEnvForReturn res toDrop fs

updateEnvForReturn :: Variable -> Int -> [CallFrame] -> VMM ()
updateEnvForReturn res toDrop fs =
    modify (\e -> e {
        envStack = res : drop toDrop (envStack e), envFrames = fs })
    >> run

executeClass :: VMM ()
executeClass = do
    name <- readString
    let cls = Class name Map.empty Nothing
        var = Variable VarClass (VObj $ OClass cls)
    modify $ \env -> env {
        envGlobals = Map.insert name var (envGlobals env) }
    push var

executeInherit :: VMM ()
executeInherit = do
    superclass <- peek 1
    handleInherit (varValue superclass)

handleInherit :: Value -> VMM ()
handleInherit (VObj (OClass super)) = do
    cls <- peekClass 0
    let newClass = cls { classSuperclass = Just super }
        var = Variable VarClass (VObj (OClass newClass))
    modify $ \e -> e {
        envStack = var : drop 2 (envStack e) }
    updateClassInGlobals newClass
    void pop
handleInherit _ = runtimeError "Superclass must be a class."

executeMethod :: VMM ()
executeMethod = do
    name <- readString
    method <- pop
    clsWrapper <- peek 0
    handleMethodDefinition name method clsWrapper

handleMethodDefinition :: T.Text -> Variable -> Variable -> VMM ()
handleMethodDefinition name method (Variable _ (VObj (OClass cls))) =
    let newMethods = Map.insert name method (classMethods cls)
        newClass = cls { classMethods = newMethods }
        var = Variable VarClass (VObj $ OClass newClass)
    in updateClassInGlobals newClass >> void pop >>
        push var
handleMethodDefinition _ _ _ = runtimeError "Expected class on stack"

updateClassInGlobals :: Class -> VMM ()
updateClassInGlobals cls = modify $ \env -> env {
    envGlobals = Map.insert (className cls)
        (Variable VarClass (VObj $ OClass cls))
        (envGlobals env)
    }

peekClass :: Int -> VMM Class
peekClass distance = do
    value <- peek distance
    case varValue value of
        VObj (OClass cls) -> return cls
        _ -> runtimeError "Expected a class."

updateFrameIp :: Int -> CallFrame -> CallFrame
updateFrameIp newIp frame = frame { frameIp = newIp }

handleBinaryOperation :: (Double -> Double -> Double) -> String -> VMM ()
handleBinaryOperation op errMsg = do
    result <- binaryOp op
    case result of
        InterpretOk -> return ()
        err -> runtimeError $ errMsg <> show err

handleBinaryOperationWithFPECheck :: (Double -> Double -> Double) -> String -> VMM ()
handleBinaryOperationWithFPECheck op errMsg = do
    (_, b) <- getBinaryOperands
    case b of
        0 -> runtimeError "Cannot divide by zero."
        _ -> do
            result <- binaryOp op
            case result of
                InterpretOk -> return ()
                err -> runtimeError $ errMsg <> show err

handleComparison :: (Double -> Double -> Bool) -> String -> VMM ()
handleComparison op errMsg = do
    b <- pop
    a <- pop
    case (varValue a, varValue b) of
        (VNumber n1, VNumber n2) ->
            let var = Variable VarBool (VBool $ op n1 n2)
            in push var
        _ -> runtimeError errMsg
