{-
-- EPITECH PROJECT, 2025
-- haskell
-- File description:
-- Compiler
-}

module Compiler (
    compile,
    CompilerState(..),
    Compiler(..),
    CompilerM,
    FunctionType(..),
    ParseRule(..),
    Precedence(..),
    ClassCompiler(..),
    Local(..),
    Upvalue(..),
    initCompilerState,
    initCompiler,
    advance,
    consume,
    consumeGet,
    match,
    check,
    runCompilerM,
    compileSource,
    debugPrint,
    errorAt,
    buildErrorMessage,
    formatTokenContext,
    errorAtCurrent,
    errorAtPrevious,
    beginScope,
    endScope,
    popLocals,
    emitPops,
    addLocal,
    declareVariable,
    checkLocalExists,
    resolveLocal,
    resolveLocalHelper,
    reportUninitializedVariable,
    markInitialized,
    currentChunk,
    updateCurrentChunk,
    emitByte,
    emitBytes,
    emitReturn,
    endCompiler,
    extractCurrentFunction,
    restoreEnclosingCompiler,
    debugFunctionIfNeeded,
    debugFunction,
    getFunctionName,
    function,
    parseFunctionParameters,
    parseParameters,
    parseIdentifier,
    parseVariable,
    addParameter,
    parseFunctionBody,
    endFunction,
    defaultDebugTime,
    optimizeFunction,
    handleInterpretOk,
    getStdoutOutput,
    getStackTop,
    generateSourceParts,
    compileAndAdjust,
    adjustChunkCode,
    getTypeTuples,
    getElemType,
    tupleDeclaration,
    classDeclaration,
    initializeClassCompiler,
    parseClassBody,
    handleSuperclass,
    checkInheritance,
    setupSuperclassScope,
    updateClassCompiler,
    finalizeClassCompiler,
    classEndScope,
    checkRightBrace,
    checkEOF,
    whileM_,
    accumulateVariable,
    accumulateType,
    method,
    funDeclaration,
    statement,
    blockStatement,
    emitEmptyTuple,
    defaultValue,
    listDeclaration,
    listGetType,
    parseList,
    addListElem,
    getVariableType,
    checkVariableType,
    varDeclaration,
    identifierConstant,
    emitVarType,
    defineVariable,
    makeConstant,
    expression,
    parsePrecedence,
    argumentList,
    argumentListTail,
    emitConstant,
    resolveUpvalue,
    emitLoop,
    emitJump,
    emitBinaryOperator,
    patchJump,
    while,
    declaration,
    tryMatchDecl,
    setupClassCompiler,
    handleTupleInitialization,
    addToList,
    handleListInitialization,
    getRule,
    binary,
    unary,
    call,
    grouping,
    literal,
    variable,
    namedVariable,
    dot,
    string,
    number,
    list,
    ternary,
    and_,
    or_,
    super_,
    this_,
    returnStatement,
    handleInitializerReturn,
    handleFunctionReturn,
    parseCondition,
    handleFalseBranch,
    handleTrueBranch,
    handleElseBranch,
    parseElseBranch,
    getNumberType,
    listAction,
    listActionGet,
    setOp,
    handlePropertyCall,
    getProperty,
    canAssignProperty,
    setProperty,
    invokeProperty,
    checkSuperUsage,
    superInvoke,
    superGet,
    addUpvalue,
    addNewUpvalue,
    findExistingUpvalue,
    markCaptured,
    updateAt,
    listActionSet,
    getMapType,
    safeMapDeclaration,
    mapDeclaration,
    initializeMap,
    handleMapInitialization
) where

import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Monad.State
    ( gets, modify, runState, MonadState(get, put), State )
import Data.Word (Word8)
import Control.Monad (unless, when, replicateM_, forM_, liftM2)
import Data.Maybe (isJust, fromJust, fromMaybe, isNothing)
import Text.Read (readMaybe)
import Types.Base (Chunk(..), Function(..), Variable(..), Value(..), VarType(..), Object(..))
import qualified Scanner as S
import qualified Chunk as C
import Debug.Debug (disassembleChunk)
import Data.Bits (Bits(..))
import Data.List (findIndex)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Applicative ((<|>), Alternative (empty))
import Preprocessor (preprocess)
import VM (initVM, interpret, InterpretResult (..), VMEnv (..))
import Data.Time (UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (secondsToDiffTime)

data Compiler = Compiler
    { compilerEnclosing :: Maybe Compiler
    , compilerFunction :: Function
    , compilerFunctionType :: FunctionType
    , compilerLocals :: [Local]
    , compilerLocalCount :: Int
    , compilerUpvalues :: [Upvalue]
    , compilerScopeDepth :: Int
    } deriving (Show, Eq)

data CompilerState = CompilerState
    { current :: Compiler
    , scanner :: S.Scanner
    , currentToken :: S.Token
    , previous :: S.Token
    , hadError :: Bool
    , panicMode :: Bool
    , currentClass :: Maybe ClassCompiler
    , debugMode :: Bool
    , optiMode :: Bool
    , debugBuffer :: [T.Text]
    } deriving (Show, Eq)

data Local = Local
    { localName :: S.Token
    , localDepth :: Int
    , localIsCaptured :: Bool
    } deriving (Show, Eq)

data Upvalue = Upvalue
    { upvalueIndex :: Word8
    , upvalueIsLocal :: Bool
    } deriving (Show, Eq)

data FunctionType
    = TypeFunction
    | TypeInitializer
    | TypeMethod
    | TypeScript
    deriving (Show, Eq)

data ClassCompiler = ClassCompiler
    { classEnclosing :: Maybe ClassCompiler
    , classHasSuperclass :: Bool
    } deriving (Show, Eq)

data Precedence
    = PrecNone
    | PrecAssignment
    | PrecOr
    | PrecAnd
    | PrecEq
    | PrecComp
    | PrecTerm
    | PrecFactor
    | PrecUnary
    | PrecCall
    | PrecPrimary
    deriving (Show, Eq, Ord, Enum)

data ParseRule = ParseRule
    { prefix :: Maybe (CompilerM ())
    , infix_ :: Maybe (CompilerM ())
    , precedence :: Precedence
    }

type CompilerM = MaybeT (State CompilerState)

initCompiler :: FunctionType -> Maybe Compiler -> Compiler
initCompiler type_ enclosing = Compiler
    { compilerEnclosing = enclosing
    , compilerFunction = Function 0 0 C.initChunk Nothing
    , compilerFunctionType = type_
    , compilerLocals = []
    , compilerLocalCount = 0
    , compilerUpvalues = []
    , compilerScopeDepth = 0
    }

initCompilerState :: T.Text -> Bool -> Bool -> CompilerState
initCompilerState source debug opti = CompilerState
    { current = initCompiler TypeScript Nothing
    , scanner = S.initScanner source
    , currentToken = S.Token S.TokenError T.empty 0 0
    , previous = S.Token S.TokenError T.empty 0 0
    , hadError = False, panicMode = False
    , currentClass = Nothing
    , debugMode = debug, debugBuffer = []
    , optiMode = opti
    }

compile :: T.Text -> Bool -> Bool -> (Maybe Function, [T.Text])
compile source debug opti =
    let preprocessedSource = preprocess source
        initialState = initCompilerState preprocessedSource debug opti
        (_, endState) = runCompilerM compileSource initialState
    in if hadError endState
        then (Nothing, debugBuffer endState)
        else (Just $ compilerFunction $ current endState, debugBuffer endState)

runCompilerM :: CompilerM a -> CompilerState -> (Maybe a, CompilerState)
runCompilerM action = runState (runMaybeT action)

compileSource :: CompilerM Function
compileSource =
    advance >>
    while (not <$> match S.TokenEOF) declaration >>
    endCompiler

advance :: CompilerM ()
advance = do
    state <- get
    let (newToken, newScanner) = runState S.scanToken (scanner state)
    put $ state { previous = currentToken state
                , currentToken = newToken
                , scanner = newScanner }

consume :: S.TokenType -> String -> CompilerM ()
consume tokenType message = do
    state <- get
    if S.tokenType (currentToken state) == tokenType
        then advance
        else errorAtCurrent message

consumeGet :: S.TokenType -> String -> CompilerM S.Token
consumeGet tokenType message = consume tokenType message >> gets previous

match :: S.TokenType -> CompilerM Bool
match expectedType = do
    state <- get
    if S.tokenType (currentToken state) == expectedType
        then advance >> return True
        else return False

check :: S.TokenType -> CompilerM Bool
check tokenType = do
    state <- get
    return $ S.tokenType (currentToken state) == tokenType

debugPrint :: T.Text -> CompilerM ()
debugPrint text = modify $ \s -> s { debugBuffer = debugBuffer s ++ [text] }

errorAt :: S.Token -> T.Text -> CompilerM ()
errorAt token message = do
    state <- get
    unless (panicMode state) $
        let errMsg = buildErrorMessage token message
        in modify $ \s -> s {
            debugBuffer = debugBuffer s ++ [errMsg],
            hadError = True,
            panicMode = True
        }

buildErrorMessage :: S.Token -> T.Text -> T.Text
buildErrorMessage token message =
    T.concat
        [ T.pack "[line ", T.pack (show $ S.tokenLine token), T.pack ", col ",
        T.pack (show $ S.tokenColumn token), T.pack "] Error",
          formatTokenContext token, T.pack ": ", message ]

formatTokenContext :: S.Token -> T.Text
formatTokenContext token = case S.tokenType token of
    S.TokenEOF   -> T.pack " at end"
    S.TokenError -> T.empty
    _            -> T.concat [T.pack " at '", S.tokenLexeme token, T.pack "'"]

errorAtCurrent :: String -> CompilerM ()
errorAtCurrent message = do
    state <- get
    errorAt (currentToken state) $ T.pack message

errorAtPrevious :: String -> CompilerM ()
errorAtPrevious message = do
    state <- get
    errorAt (previous state) $ T.pack message

beginScope :: CompilerM ()
beginScope = modifyCurrentCompiler $ \c ->
    c { compilerScopeDepth = compilerScopeDepth c + 1 }

endScope :: CompilerM ()
endScope = do
    compiler <- gets current
    let depth = compilerScopeDepth compiler
    let locals = compilerLocals compiler
    (pops, remaining) <- popLocals locals depth
    emitPops pops
    modifyCurrentCompiler $ \c -> c
        { compilerLocals = remaining, compilerScopeDepth = depth - 1 }

popLocals :: [Local] -> Int -> CompilerM (Int, [Local])
popLocals locals depth =
    let (toRemove, keep) = span (\l -> localDepth l >= depth) (reverse locals)
        numPops = length toRemove
    in return (numPops, reverse keep)

emitPops :: Int -> CompilerM ()
emitPops n = replicateM_ n (emitByte $ C.toByte C.OpPop)

addLocal :: S.Token -> CompilerM ()
addLocal name = do
    compiler <- gets current
    when (length (compilerLocals compiler) >= 256) $
        errorAtPrevious "Too many local variables in function."
    let depth = max (compilerScopeDepth compiler) 0
    let local = Local name depth False
    modifyCurrentCompiler $ \c ->
        c { compilerLocals = compilerLocals c ++ [local] }

declareVariable :: CompilerM ()
declareVariable = do
    compiler <- gets current
    when (compilerScopeDepth compiler > 0) $ do
        name <- gets previous
        checkLocalExists name
        addLocal name

checkLocalExists :: S.Token -> CompilerM ()
checkLocalExists name = do
    compiler <- gets current
    let locals = compilerLocals compiler
    forM_ (reverse locals) $ \local -> when (localDepth local == -1
        || localDepth local >= compilerScopeDepth compiler) $
        when (localName local == name) $
            errorAtPrevious "Already a variable with this name in this scope."

resolveLocal :: S.Token -> CompilerM (Maybe Word8)
resolveLocal name = do
    locals <- gets (compilerLocals . current)
    resolveLocalHelper name (zip [1 :: Int ..] locals)

resolveLocalHelper :: S.Token -> [(Int, Local)] -> CompilerM (Maybe Word8)
resolveLocalHelper _ [] = return Nothing
resolveLocalHelper name ((i, local):rest)
    | S.tokenLexeme (localName local) == S.tokenLexeme name =
        if localDepth local == -1
            then reportUninitializedVariable >> return Nothing
            else return $ Just $ fromIntegral i
    | otherwise = resolveLocalHelper name rest

reportUninitializedVariable :: CompilerM ()
reportUninitializedVariable =
    errorAtPrevious "Can't read local variable in its own initializer."

markInitialized :: CompilerM ()
markInitialized = do
    comp <- gets current
    when (compilerScopeDepth comp > 0 && not (null (compilerLocals comp))) $
        modifyCurrentCompiler $ \c -> c { compilerLocals =
            updateLastLocal (compilerLocals c) (compilerScopeDepth c) }
    where
        updateLastLocal locals depth =
            let (init', last') = splitAt (length locals - 1) locals
                lastLocal = head last'
            in init' ++ [lastLocal { localDepth = depth }]

currentChunk :: CompilerM Chunk
currentChunk = gets (functionChunk . compilerFunction . current)

updateCurrentChunk :: Chunk -> CompilerM ()
updateCurrentChunk chunk = modifyCurrentCompiler $ \c ->
    c { compilerFunction = (compilerFunction c) { functionChunk = chunk } }

emitByte :: Word8 -> CompilerM ()
emitByte byte = do
    chunk <- currentChunk
    state <- get
    let line = S.tokenLine $ previous state
    let newChunk = C.writeChunk chunk byte line
    updateCurrentChunk newChunk

emitBytes :: Word8 -> Word8 -> CompilerM ()
emitBytes byte1 byte2 = emitByte byte1 >> emitByte byte2

emitReturn :: CompilerM ()
emitReturn = do
    compiler <- gets current
    case compilerFunctionType compiler of
        TypeInitializer ->
            emitBytes (C.toByte C.OpGetLocal) 0
        _ ->
            emitByte $ C.toByte C.OpNil
    emitByte $ C.toByte C.OpReturn

endCompiler :: CompilerM Function
endCompiler = do
    emitReturn
    func <- extractCurrentFunction
    restoreEnclosingCompiler
    debugFunctionIfNeeded func
    return func

extractCurrentFunction :: CompilerM Function
extractCurrentFunction = gets (compilerFunction . current)

restoreEnclosingCompiler :: CompilerM ()
restoreEnclosingCompiler = do
    enclosing <- gets (compilerEnclosing . current)
    when (isJust enclosing) $
        modify $ \s -> s { current = fromJust enclosing }

debugFunctionIfNeeded :: Function -> CompilerM ()
debugFunctionIfNeeded func = do
    state <- get
    let shouldDebug = debugMode state && not (hadError state)
    when shouldDebug $
        debugFunction func (compilerFunctionType (current state))

debugFunction :: Function -> FunctionType -> CompilerM ()
debugFunction func funcType =
    let name = getFunctionName func funcType
        debugOutput = disassembleChunk (functionChunk func) name
    in debugPrint debugOutput

getFunctionName :: Function -> FunctionType -> T.Text
getFunctionName _ TypeScript = T.pack "script"
getFunctionName func _ = fromMaybe (T.pack "<fn>") (functionName func)

while :: CompilerM Bool -> CompilerM () -> CompilerM ()
while condition action = do
    shouldContinue <- condition
    when shouldContinue $ action >> while condition action

accumulateVariable :: CompilerM Bool -> CompilerM Variable -> CompilerM [Variable] -> CompilerM [Variable]
accumulateVariable condition action acc = do
    shouldContinue <- condition
    if shouldContinue
        then do
            value <- action
            accValues <- acc
            accumulateVariable condition action (return (accValues ++ [value]))
        else acc

accumulateType :: CompilerM Bool -> CompilerM VarType -> CompilerM [VarType] -> CompilerM [VarType]
accumulateType condition action acc = do
    shouldContinue <- condition
    if shouldContinue
        then do
            value <- action
            accValues <- acc
            accumulateType condition action (return (accValues ++ [value]))
        else acc

declaration :: CompilerM ()
declaration = tryMatchDecl S.TokenClass classDeclaration
          <|> tryMatchDecl S.TokenFun funDeclaration
          <|> tryMatchDecl S.TokenList listDeclaration
          <|> tryMatchDecl S.TokenTuple tupleDeclaration
          <|> tryMatchDecl S.TokenSafeMap safeMapDeclaration
          <|> tryMatchDecl S.TokenMap mapDeclaration
          <|> tryMatchDecl S.TokenVar varDeclaration
          <|> statement

tryMatchDecl :: S.TokenType -> CompilerM () -> CompilerM ()
tryMatchDecl tokenType action = do
    originalState <- get
    matched <- match tokenType
    if matched
        then action
        else put originalState >>
            whenM (gets panicMode) synchronize >>
            empty

statement :: CompilerM ()
statement = tryMatch S.TokenPrint printStatement
        <|> tryMatch S.TokenPrintLn printlnStatement
        <|> tryMatch S.TokenSetRecursionLimit recursionLimitStatement
        <|> tryMatch S.TokenFor forStatement
        <|> tryMatch S.TokenIf ifStatement
        <|> tryMatch S.TokenReturn returnStatement
        <|> tryMatch S.TokenWhile whileStatement
        <|> tryMatch S.TokenLeftBrace blockStatement
        <|> expressionStatement

tryMatch :: S.TokenType -> CompilerM () -> CompilerM ()
tryMatch token action = do
    originalState <- get
    matched <- match token
    if matched
        then action
        else put originalState *> empty

blockStatement :: CompilerM ()
blockStatement = beginScope >> block >> endScope

classDeclaration :: CompilerM ()
classDeclaration = do
    classNameToken <- consumeGet S.TokenIdentifier "Expect class name."
    let className = S.tokenLexeme classNameToken
    nameConstant <- identifierConstant classNameToken
    declareVariable
    emitBytes (C.toByte C.OpClass) nameConstant
    defineVariable nameConstant VarClass
    setupClassCompiler classNameToken className

setupClassCompiler :: S.Token -> T.Text -> CompilerM ()
setupClassCompiler classNameToken className =
    initializeClassCompiler >>
    handleSuperclass classNameToken className >>
    namedVariable classNameToken False >>
    parseClassBody >>
    finalizeClassCompiler

initializeClassCompiler :: CompilerM ()
initializeClassCompiler = do
    currentCls <- gets currentClass
    let classCompiler = ClassCompiler {
        classHasSuperclass = False, classEnclosing = currentCls }
    modify $ \s -> s { currentClass = Just classCompiler }

parseClassBody :: CompilerM ()
parseClassBody =
    consume S.TokenLeftBrace "Expect '{' before class body." >>
    whileM_ (liftM2 (&&) (not <$> checkRightBrace) (not <$> checkEOF))
        method >>
    consume S.TokenRightBrace "Expect '}' after class body." >>
    emitByte (C.toByte C.OpPop)

handleSuperclass :: S.Token -> T.Text -> CompilerM ()
handleSuperclass classNameToken className = do
    hasLess <- match S.TokenColon
    when hasLess $ do
        superClassNameToken <-
            consumeGet S.TokenIdentifier "Expect superclass name."
        namedVariable superClassNameToken False
        checkInheritance className superClassNameToken
        setupSuperclassScope classNameToken

checkInheritance :: T.Text -> S.Token -> CompilerM ()
checkInheritance className superClassNameToken =
    when (S.tokenLexeme superClassNameToken == className)
        (errorAtCurrent "A class can't inherit from itself.")

setupSuperclassScope :: S.Token -> CompilerM ()
setupSuperclassScope classNameToken =
    beginScope >>
    addLocal (syntheticToken $ T.pack "super") >>
    defineVariable 0 VarClass >>
    namedVariable classNameToken False >>
    emitByte (C.toByte C.OpInherit) >>
    updateClassCompiler

updateClassCompiler :: CompilerM ()
updateClassCompiler = do
    currentCC <- gets currentClass
    case currentCC of
        Just cc -> modify $ \s -> s
            { currentClass = Just cc { classHasSuperclass = True } }
        Nothing -> errorAtCurrent "No class compiler found."

finalizeClassCompiler :: CompilerM ()
finalizeClassCompiler = do
    currentCC <- gets currentClass
    case currentCC of
        Just cc -> when (classHasSuperclass cc) classEndScope >>
            modify (\s -> s { currentClass = classEnclosing cc })
        Nothing -> errorAtCurrent "No class compiler found."

classEndScope :: CompilerM ()
classEndScope = do
    compiler <- gets current
    let depth = compilerScopeDepth compiler
    modifyCurrentCompiler $ \c -> c
        { compilerScopeDepth = depth - 1 }

checkRightBrace :: CompilerM Bool
checkRightBrace = check S.TokenRightBrace

checkEOF :: CompilerM Bool
checkEOF = check S.TokenEOF

whileM_ :: Monad m => m Bool -> m () -> m ()
whileM_ condition action = do
    shouldContinue <- condition
    when shouldContinue $ action >> whileM_ condition action

method :: CompilerM ()
method = do
    methodNameToken <- consumeGet S.TokenIdentifier "Expect method name."
    let methodName = S.tokenLexeme methodNameToken
    let funcType = if methodName == T.pack "init"
                    then TypeInitializer
                    else TypeMethod
    function funcType
    methodConstant <- identifierConstant methodNameToken
    emitBytes (C.toByte C.OpMethod) methodConstant

funDeclaration :: CompilerM ()
funDeclaration = do
    funcName <- parseVariable "Expect function name."
    markInitialized
    function TypeFunction
    defineVariable funcName VarAny

function :: FunctionType -> CompilerM ()
function type_ = do
    state <- get
    let enclosing = current state
    let newCompiler = initCompiler type_ (Just enclosing)
    put $ state { current = newCompiler }
    beginScope
    parseFunctionParameters
    parseFunctionBody
    endFunction enclosing

parseFunctionParameters :: CompilerM ()
parseFunctionParameters = do
    consume S.TokenLeftParen "Expect '(' after function name."
    hasRightParen <- check S.TokenRightParen
    if not hasRightParen
        then parseParameters >>
            consume S.TokenRightParen "Expect ')' after parameters."
        else advance

parseParameters :: CompilerM ()
parseParameters = addParameter >> while (match S.TokenComma) addParameter

parseIdentifier :: String -> CompilerM S.Token
parseIdentifier = consumeGet S.TokenIdentifier

parseVariable :: String -> CompilerM Word8
parseVariable errorMessage = do
    consume S.TokenIdentifier errorMessage
    declareVariable
    compiler <- gets current
    if compilerScopeDepth compiler > 0
        then return 0
        else identifierConstant =<< gets previous

addParameter :: CompilerM ()
addParameter = do
    compiler <- gets current
    let func = compilerFunction compiler
    let newFunc = func { functionArity = functionArity func + 1 }
    when (functionArity newFunc > 255) $
        errorAtCurrent "Can't have more than 255 parameters."
    modifyCurrentCompiler $ \c -> c { compilerFunction = newFunc }
    name <- consumeGet S.TokenIdentifier "Expect parameter name."
    addLocal name
    markInitialized

parseFunctionBody :: CompilerM ()
parseFunctionBody =
    consume S.TokenLeftBrace "Expect '{' before function body." >> block

endFunction :: Compiler -> CompilerM ()
endFunction enclosing = do
    opti <- gets optiMode
    func <- endCompiler
    func' <- if opti then optimizeFunction func else return func
    modify $ \s -> s { current = enclosing }
    constant <- makeConstant (Variable VarAny (VObj $ OFunction func'))
    emitBytes (C.toByte C.OpClosure) constant
    compiler <- gets current
    forM_ (compilerUpvalues compiler) $ \up ->
        emitBytes (if upvalueIsLocal up then 1 else 0) (upvalueIndex up)

defaultDebugTime :: UTCTime
defaultDebugTime = UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 0)

optimizeFunction :: Function -> CompilerM Function
optimizeFunction func =
    let env = initVM False defaultDebugTime
        (result, updatedEnv) = interpret func env
    in if result == InterpretOk
        then handleInterpretOk updatedEnv func
        else return func

handleInterpretOk :: VMEnv -> Function -> CompilerM Function
handleInterpretOk updatedEnv func =
    let stdoutOutput = getStdoutOutput updatedEnv
        stackTop = getStackTop updatedEnv
        sourceParts = generateSourceParts stdoutOutput stackTop
    in if not (null sourceParts)
        then compileAndAdjust sourceParts stackTop func
        else return func

getStdoutOutput :: VMEnv -> String
getStdoutOutput env = T.unpack $ T.concat $ envStdoutBuffer env

getStackTop :: VMEnv -> Maybe Variable
getStackTop env = case envStack env of
    (top:_) -> Just top
    [] -> Nothing

generateSourceParts :: String -> Maybe Variable -> [String]
generateSourceParts stdoutOutput stackTop =
    let toPrint = [(\line -> "print " ++ show line ++ ";") stdoutOutput]
        stackTopStatement = case stackTop of
            Just top -> [show top ++ ";"]
            Nothing -> []
    in toPrint ++ stackTopStatement

compileAndAdjust :: [String] -> Maybe Variable -> Function -> CompilerM Function
compileAndAdjust sourceParts stackTop func = do
    enclosing <- gets (compilerEnclosing . current)
    isClassMethod <- gets currentClass
    if isNothing isClassMethod then
        let newSource = T.pack $ unlines sourceParts
            (compiledFunc, _) = compile newSource False False
        in case compiledFunc of
            Just newFunc -> return $ adjustChunkCode newFunc stackTop enclosing
            Nothing -> return func
        else return func

adjustChunkCode :: Function -> Maybe Variable -> Maybe Compiler -> Function
adjustChunkCode func (Just _) Nothing =
    let chunk = functionChunk func
        modifiedChunkCode = take (length (chunkCode chunk) - 3)
            (chunkCode chunk) ++ [34]
        newChunk = chunk {chunkCode = modifiedChunkCode}
    in func {functionChunk = newChunk}
adjustChunkCode func _ _ = func

getMapType :: CompilerM (VarType, VarType)
getMapType = do
    keyType <- getOneVarType
    consume S.TokenColon "Need to have a value type for the map."
    valueType <- getOneVarType
    hasOther <- check S.TokenColon
    if hasOther
        then errorAtCurrent "Can't be more that 2 type in a map." >>
            return (VarAny, VarAny)
        else return (keyType, valueType)

safeMapDeclaration :: CompilerM ()
safeMapDeclaration = initializeMap True

mapDeclaration :: CompilerM ()
mapDeclaration = initializeMap False

initializeMap :: Bool -> CompilerM ()
initializeMap safe = do
    consume S.TokenLeftSqBrace "Expect '[' in list declaration."
    (keyType, valueType) <- getMapType
    consume S.TokenRightSqBrace "Expect ']' in list declaration."
    name <- parseVariable "Expect variable name."
    handleMapInitialization safe keyType valueType name

handleMapInitialization :: Bool -> VarType -> VarType -> Word8 -> CompilerM ()
handleMapInitialization safe keyType valueType name = do
    hasEqual <- match S.TokenEqual
    let mapType = VarMap keyType valueType safe
    if hasEqual
        then errorAtCurrent "Can't initialize a Map currently ...!"
        else
            let var = Variable mapType (VObj $ OMap Map.empty)
            in emitConstant var
    consume S.TokenSemicolon "Expect ';' after variable declaration."
    defineVariable name mapType

getTypeTuples :: CompilerM [VarType]
getTypeTuples = do
    firstType <- getOneVarType
    accumulateType (match S.TokenComma) getOneVarType (return [firstType])

getOneVarType :: CompilerM VarType
getOneVarType = do
    advance
    token <- gets previous
    getElemType (S.tokenType token) token

getElemType :: S.TokenType -> S.Token -> CompilerM VarType
getElemType _ token =
    case listGetType (S.tokenLexeme token) of
        Just type_ -> return type_
        Nothing -> errorAtCurrent "Bad type in tuple." >> return VarAny

tupleDeclaration :: CompilerM ()
tupleDeclaration = do
    consume S.TokenLeftSqBrace "Expect '[' in list declaration."
    tupleTypes <- getTypeTuples
    consume S.TokenRightSqBrace "Expect ']' in list declaration."
    name <- parseVariable "Expect variable name."
    handleTupleInitialization tupleTypes name

handleTupleInitialization :: [VarType] -> Word8 -> CompilerM ()
handleTupleInitialization tupleTypes name = do
    hasEqual <- match S.TokenEqual
    if hasEqual
        then expression
        else emitEmptyTuple tupleTypes
    consume S.TokenSemicolon "Expect ';' after variable declaration."
    defineVariable name (VarTuple tupleTypes)

emitEmptyTuple :: [VarType] -> CompilerM ()
emitEmptyTuple tupleTypes =
    let defaultList = map defaultValue tupleTypes
        var = Variable (VarTuple tupleTypes) (VObj $ OList defaultList)
    in emitConstant var

defaultValue :: VarType -> Variable
defaultValue VarInteger = Variable VarInteger (VNumber 0.0)
defaultValue VarString  = Variable VarString (VObj $ OString $ T.pack "")
defaultValue VarBool    = Variable VarBool (VBool False)
defaultValue VarDouble  = Variable VarDouble (VNumber 0.0)
defaultValue _          = Variable VarAny VNil

listDeclaration :: CompilerM ()
listDeclaration = do
    consume S.TokenLeftSqBrace "Expect '[' in list declaration."
    tokenListType <- consumeGet S.TokenIdentifier "Expect type in bracket."
    case listGetType (S.tokenLexeme tokenListType) of
        Nothing -> errorAtCurrent "Invalid list type in declaration."
        Just validListType -> do
            consume S.TokenRightSqBrace "Expect ']' in list declaration."
            name <- parseVariable "Expect variable name."
            handleListInitialization validListType name

handleListInitialization :: VarType -> Word8 -> CompilerM ()
handleListInitialization listType name = do
    hasEqual <- match S.TokenEqual
    if hasEqual
        then expression
        else
            let var = Variable listType (VObj $ OList [])
            in emitConstant var
    consume S.TokenSemicolon "Expect ';' after variable declaration."
    defineVariable name listType

listGetType :: T.Text -> Maybe VarType
listGetType listType = case T.unpack listType of
    "Int"    -> Just VarInteger
    "Bool"   -> Just VarBool
    "String" -> Just VarString
    "Double" -> Just VarDouble
    _        -> Nothing

parseList :: CompilerM [Variable]
parseList = do
    firstValue <- addListElem
    accumulateVariable (match S.TokenComma) addListElem (return [firstValue])

addListElem :: CompilerM Variable
addListElem = do
    advance
    token <- gets previous
    addToList (S.tokenType token) token

addToList :: S.TokenType -> S.Token -> CompilerM Variable
addToList S.TokenNumber token =
    let lexeme = S.tokenLexeme token
    in case readMaybe (T.unpack lexeme) :: Maybe Double of
        Just value -> do
            type_ <- getNumberType value
            return (Variable type_ (VNumber value))
        Nothing -> return (Variable VarAny VNil)
addToList S.TokenTrue _ = return (Variable VarBool (VBool True))
addToList S.TokenFalse _ = return (Variable VarBool (VBool False))
addToList S.TokenString token =
    return (Variable VarString (VObj (OString (S.tokenLexeme token))))
addToList t _ =
    errorAtCurrent ("Can't have this type in list." ++ show t) >>
    return (Variable VarAny VNil)

getVariableType :: CompilerM VarType
getVariableType = do
    advance
    token <- gets previous
    case T.unpack (S.tokenLexeme token) of
        "Int"     -> return VarInteger
        "Bool"    -> return VarBool
        "String"  -> return VarString
        "Double"  -> return VarDouble
        _         -> errorAtCurrent "Invalid variable type." >> return VarAny

checkVariableType :: CompilerM VarType
checkVariableType = do
    hasColon <- match S.TokenColon
    if hasColon
        then getVariableType
        else return VarAny

varDeclaration :: CompilerM ()
varDeclaration = do
    name <- parseVariable "Expect variable name."
    variableType <- checkVariableType
    hasEqual <- match S.TokenEqual
    if hasEqual
        then expression
        else emitByte $ C.toByte C.OpNil
    consume S.TokenSemicolon "Expect ';' after variable declaration."
    defineVariable name variableType

identifierConstant :: S.Token -> CompilerM Word8
identifierConstant token =
    let str = VObj $ OString $ S.tokenLexeme token
        var = Variable VarString str
    in makeConstant var

emitVarType :: VarType -> CompilerM ()
emitVarType (VarTuple types) =
    emitByte (C.varTypeToWord8 (VarTuple types)) >>
    emitByte (fromIntegral (length types)) >>
    mapM_ emitVarType types
emitVarType (VarMap k e s) =
    emitByte (C.varTypeToWord8 (VarMap k e s)) >>
    emitByte (C.varTypeToWord8 k) >>
    emitByte (C.varTypeToWord8 e) >>
    emitByte (fromIntegral (fromEnum s))
emitVarType type_ = emitByte (C.varTypeToWord8 type_)

defineVariable :: Word8 -> VarType -> CompilerM ()
defineVariable var type_ = do
    compiler <- gets current
    if compilerScopeDepth compiler > 0
        then markInitialized
        else
            emitBytes (C.toByte C.OpDefineGlobal) var >>
            emitVarType type_

makeConstant :: Variable -> CompilerM Word8
makeConstant var = do
    chunk <- currentChunk
    let (newChunk, constant) = C.addConstant chunk var
    updateCurrentChunk newChunk
    when (constant > 255) $
        errorAtPrevious "Too many constants in one chunk."
    return $ fromIntegral constant

expression :: CompilerM ()
expression = parsePrecedence PrecAssignment

parsePrecedence :: Precedence -> CompilerM ()
parsePrecedence prec = do
    advance
    prefixRule <- gets (prefix . getRule . S.tokenType . previous)
    case prefixRule of
        Nothing -> errorAtPrevious "Expect expression."
        Just rule -> rule >> parseInfixRules prec

parseInfixRules :: Precedence -> CompilerM ()
parseInfixRules prec =
    while (shouldContinueParsing prec) $ do
        advance
        infixRule <- gets (infix_ . getRule . S.tokenType . previous)
        fromMaybe (return ()) infixRule

shouldContinueParsing :: Precedence -> CompilerM Bool
shouldContinueParsing prec = do
    currentTok <- gets currentToken
    let currentType = S.tokenType currentTok
    let rule = getRule currentType
    return $ prec <= precedence rule

getRule :: S.TokenType -> ParseRule
getRule S.TokenLeftParen    = ParseRule (Just grouping) (Just call) PrecCall
getRule S.TokenDot          = ParseRule Nothing       (Just dot)    PrecCall
getRule S.TokenMinus        = ParseRule (Just unary)  (Just binary) PrecTerm
getRule S.TokenPlus         = ParseRule Nothing       (Just binary) PrecTerm
getRule S.TokenSlash        = ParseRule Nothing       (Just binary) PrecFactor
getRule S.TokenStar         = ParseRule Nothing       (Just binary) PrecFactor
getRule S.TokenModulo       = ParseRule Nothing       (Just binary) PrecTerm
getRule S.TokenBang         = ParseRule (Just unary)  Nothing       PrecNone
getRule S.TokenBangEqual    = ParseRule Nothing       (Just binary) PrecEq
getRule S.TokenEqualEqual   = ParseRule Nothing       (Just binary) PrecEq
getRule S.TokenGreater      = ParseRule Nothing       (Just binary) PrecComp
getRule S.TokenGreaterEqual = ParseRule Nothing       (Just binary) PrecComp
getRule S.TokenLess         = ParseRule Nothing       (Just binary) PrecComp
getRule S.TokenLessEqual    = ParseRule Nothing       (Just binary) PrecComp
getRule S.TokenIdentifier   = ParseRule (Just variable) Nothing     PrecNone
getRule S.TokenString       = ParseRule (Just string)   Nothing     PrecNone
getRule S.TokenNumber       = ParseRule (Just number)   Nothing     PrecNone
getRule S.TokenLeftSqBrace  = ParseRule (Just list)    Nothing      PrecNone
getRule S.TokenTernary      = ParseRule (Just ternary)  Nothing     PrecNone
getRule S.TokenAnd          = ParseRule Nothing       (Just and_)   PrecAnd
getRule S.TokenFalse        = ParseRule (Just literal) Nothing      PrecNone
getRule S.TokenNil          = ParseRule (Just literal) Nothing      PrecNone
getRule S.TokenOr           = ParseRule Nothing       (Just or_)    PrecOr
getRule S.TokenSuper        = ParseRule (Just super_)  Nothing      PrecNone
getRule S.TokenThis         = ParseRule (Just this_)   Nothing      PrecNone
getRule S.TokenTrue         = ParseRule (Just literal) Nothing      PrecNone
getRule _                   = ParseRule Nothing        Nothing      PrecNone

getImplicitType :: [Variable] -> CompilerM [VarType]
getImplicitType variables = return $ map varType variables

list :: CompilerM ()
list = do
    hasRightParen <- match S.TokenRightSqBrace
    if not hasRightParen
        then do
            newList <- parseList
            type_ <- getImplicitType newList
            consume S.TokenRightSqBrace "Expect ']' after list assignation."
            emitConstant (Variable (VarTuple type_) (VObj $ OList newList))
        else
            emitConstant (Variable (VarTuple []) (VObj $ OList []))

printStatement :: CompilerM ()
printStatement = expression >>
    consume S.TokenSemicolon "Expect ';' after value." >>
    emitByte (C.toByte C.OpPrint)

printlnStatement :: CompilerM ()
printlnStatement = expression >>
    consume S.TokenSemicolon "Expect ';' after value." >>
    emitByte (C.toByte C.OpPrintLn)

recursionLimitStatement :: CompilerM ()
recursionLimitStatement = expression >>
    consume S.TokenSemicolon "Expect ';' after value." >>
    emitByte (C.toByte C.OpSetRecursionLimit)

emitLoop :: Int -> CompilerM ()
emitLoop loopStart = do
    chunk <- currentChunk
    let offset = length (chunkCode chunk) - loopStart + 3
    when (offset > 0xFFFF) $
        errorAtPrevious "Loop body too large."
    emitByte $ C.toByte C.OpLoop
    emitByte $ fromIntegral $ (offset `shiftR` 8) .&. 0xFF
    emitByte $ fromIntegral $ offset .&. 0xFF

ifStatement :: CompilerM ()
ifStatement = do
    parseIfCondition
    thenJump <- emitJump C.OpJumpIfFalse
    emitByte $ C.toByte C.OpPop
    statement
    elseJump <- emitJump C.OpJump
    patchIfThenBranch thenJump
    handleElseBranch elseJump

parseIfCondition :: CompilerM ()
parseIfCondition =
    consume S.TokenLeftParen "Expect '(' after 'if'." >>
    expression >>
    consume S.TokenRightParen "Expect ')' after condition."

patchIfThenBranch :: Int -> CompilerM ()
patchIfThenBranch thenJump = patchJump thenJump >> emitByte (C.toByte C.OpPop)

handleElseBranch :: Int -> CompilerM ()
handleElseBranch elseJump = do
    hasElse <- match S.TokenElse
    when hasElse statement
    patchJump elseJump

returnStatement :: CompilerM ()
returnStatement = do
    compiler <- gets current
    case compilerFunctionType compiler of
        TypeScript -> errorAtPrevious "Can't return from top-level code."
        TypeInitializer -> handleInitializerReturn
        _ -> handleFunctionReturn

handleInitializerReturn :: CompilerM ()
handleInitializerReturn = do
    hasValue <- not <$> match S.TokenSemicolon
    when hasValue $ errorAtPrevious "Can't return a value from an initializer."
    emitBytes (C.toByte C.OpNil) (C.toByte C.OpReturn)

handleFunctionReturn :: CompilerM ()
handleFunctionReturn = do
    hasValue <- not <$> match S.TokenSemicolon
    if hasValue
        then expression >>
            consume S.TokenSemicolon "Expect ';' after return value." >>
            emitByte (C.toByte C.OpReturn)
        else emitBytes (C.toByte C.OpNil) (C.toByte C.OpReturn)

whileStatement :: CompilerM ()
whileStatement = do
    loopStart <- getWhileLoopStart
    parseWhileCondition
    exitJump <- emitJump C.OpJumpIfFalse
    emitByte $ C.toByte C.OpPop
    statement
    emitLoop loopStart
    patchJump exitJump
    emitByte $ C.toByte C.OpPop

getWhileLoopStart :: CompilerM Int
getWhileLoopStart =
    gets (length . chunkCode . functionChunk . compilerFunction . current)

parseWhileCondition :: CompilerM ()
parseWhileCondition =
    consume S.TokenLeftParen "Expect '(' after 'while'." >>
    expression >>
    consume S.TokenRightParen "Expect ')' after condition."

block :: CompilerM ()
block =
    while (not <$> (check S.TokenRightBrace <||> check S.TokenEOF)) declaration
    >> consume S.TokenRightBrace "Expect '}' after block."

expressionStatement :: CompilerM ()
expressionStatement = expression >>
    consume S.TokenSemicolon "Expect ';' after expression." >>
    emitByte (C.toByte C.OpPop)

(<||>) :: CompilerM Bool -> CompilerM Bool -> CompilerM Bool
a <||> b = do
    aResult <- a
    if aResult
        then return True
        else b

emitJump :: C.OpCode -> CompilerM Int
emitJump instruction = do
    emitByte $ C.toByte instruction
    emitByte 0xFF
    emitByte 0xFF
    chunk <- currentChunk
    return $ length (chunkCode chunk) - 2

patchJump :: Int -> CompilerM ()
patchJump offset = do
    chunk <- currentChunk
    let curr = length (chunkCode chunk)
        jump = curr - offset - 2
    when (jump > 0xFFFF) $ errorAtPrevious "Too much code to jump over."
    let (high, low) = splitJump jump
        updatedCode = patchCode (chunkCode chunk) offset high low
    updateCurrentChunk $ chunk { chunkCode = updatedCode }

splitJump :: Int -> (Int, Int)
splitJump jump =
    let high = (jump `shiftR` 8) .&. 0xFF
        low = jump .&. 0xFF
    in (high, low)

patchCode :: [Word8] -> Int -> Int -> Int -> [Word8]
patchCode code offset high low = take offset code ++
    [fromIntegral high, fromIntegral low] ++ drop (offset + 2) code

grouping :: CompilerM ()
grouping = expression >>
    consume S.TokenRightParen "Expect ')' after expression."

unary :: CompilerM ()
unary = do
    operatorType <- gets (S.tokenType . previous)
    parsePrecedence PrecUnary
    case operatorType of
        S.TokenMinus -> emitByte $ C.toByte C.OpNegate
        S.TokenBang -> emitByte $ C.toByte C.OpNot
        _ -> return ()

binary :: CompilerM ()
binary = do
    operatorType <- gets (S.tokenType . previous)
    let rule = getRule operatorType
    parsePrecedence (succ $ precedence rule)
    emitBinaryOperator operatorType

emitBinaryOperator :: S.TokenType -> CompilerM ()
emitBinaryOperator S.TokenPlus          = emitByte $ C.toByte C.OpAdd
emitBinaryOperator S.TokenMinus         = emitByte $ C.toByte C.OpSubtract
emitBinaryOperator S.TokenStar          = emitByte $ C.toByte C.OpMultiply
emitBinaryOperator S.TokenSlash         = emitByte $ C.toByte C.OpDivide
emitBinaryOperator S.TokenModulo        = emitByte $ C.toByte C.OpModulo
emitBinaryOperator S.TokenBangEqual     =
    emitBytes (C.toByte C.OpEqual) (C.toByte C.OpNot)
emitBinaryOperator S.TokenEqualEqual    = emitByte $ C.toByte C.OpEqual
emitBinaryOperator S.TokenGreater       = emitByte $ C.toByte C.OpGreater
emitBinaryOperator S.TokenGreaterEqual  =
    emitBytes (C.toByte C.OpLess) (C.toByte C.OpNot)
emitBinaryOperator S.TokenLess          = emitByte $ C.toByte C.OpLess
emitBinaryOperator S.TokenLessEqual     =
    emitBytes (C.toByte C.OpGreater) (C.toByte C.OpNot)
emitBinaryOperator _                    = return ()

literal :: CompilerM ()
literal = do
    operatorType <- gets (S.tokenType . previous)
    case operatorType of
        S.TokenFalse -> emitByte $ C.toByte C.OpFalse
        S.TokenTrue -> emitByte $ C.toByte C.OpTrue
        S.TokenNil -> emitByte $ C.toByte C.OpNil
        _ -> return ()

ternary :: CompilerM ()
ternary = do
    consume S.TokenLeftParen "Expect '(' after ternary expression '<->'."
    parseCondition
    consume S.TokenQuestionMark
        "Expect '?' after condition in ternary operator."
    falseJump <- handleFalseBranch
    endJump <- handleTrueBranch falseJump
    consume S.TokenColon "Expect ':' after true branch of ternary operator."
    parseElseBranch endJump

parseCondition :: CompilerM ()
parseCondition = expression >>
    consume S.TokenRightParen "Expect ')' after condition."

handleFalseBranch :: CompilerM Int
handleFalseBranch = do
    falseJump <- emitJump C.OpJumpIfFalse
    emitByte (C.toByte C.OpPop)
    expression
    return falseJump

handleTrueBranch :: Int -> CompilerM Int
handleTrueBranch falseJump = do
    endJump <- emitJump C.OpJump
    patchJump falseJump
    emitByte (C.toByte C.OpPop)
    return endJump

parseElseBranch :: Int -> CompilerM ()
parseElseBranch endJump = expression >> patchJump endJump

getNumberType :: Double -> CompilerM VarType
getNumberType n =
    if n == fromInteger (round n)
        then return VarInteger
        else return VarDouble

number :: CompilerM ()
number = do
    value <- gets (read . T.unpack . S.tokenLexeme . previous)
    type_ <- getNumberType value
    let var = Variable type_ (VNumber value)
    emitConstant var

string :: CompilerM ()
string = do
    value <- gets (S.tokenLexeme . previous)
    let var = Variable VarString (VObj $ OString value)
    emitConstant var

variable :: CompilerM ()
variable = do
    canAssign <- gets $ \s ->
        precedence (getRule $ S.tokenType $ currentToken s) <= PrecAssignment
    prev <- gets previous
    namedVariable prev canAssign

namedVariable :: S.Token -> Bool -> CompilerM ()
namedVariable name canAssign = do
    (getOp, arg) <- resolveVariable name
    hasBracket <- match S.TokenLeftSqBrace
    if hasBracket
        then listAction getOp arg
        else checkVariableAssign canAssign getOp arg

checkVariableAssign :: Bool -> C.OpCode -> Word8 -> CompilerM ()
checkVariableAssign canAssign getOp arg = do
    hasEqual <- match S.TokenEqual
    if canAssign && hasEqual
        then expression >> emitBytes (C.toByte (setOp getOp)) arg
        else emitBytes (C.toByte getOp) arg

listAction :: C.OpCode -> Word8 -> CompilerM ()
listAction getOp arg = do
    index <- addListElem
    consume S.TokenRightSqBrace "Expect ']' after list assignation."
    hasEqual <- match S.TokenEqual
    if not hasEqual
        then listActionGet index >> emitBytes (C.toByte getOp) arg
        else expression >> listActionSet index >>
            emitBytes (C.toByte (setOp getOp)) arg

listActionGet :: Variable -> CompilerM ()
listActionGet index =
    emitConstant index >>
    emitByte (C.toByte C.OpGetAtIndex)

listActionSet :: Variable -> CompilerM ()
listActionSet index =
    emitConstant index >>
    emitByte (C.toByte C.OpSetAtIndex)

setOp :: C.OpCode -> C.OpCode
setOp op = case op of
    C.OpGetLocal -> C.OpSetLocal
    C.OpGetUpvalue -> C.OpSetUpvalue
    C.OpGetGlobal -> C.OpSetGlobal
    _ -> error "Invalid get operation"

resolveVariable :: S.Token -> CompilerM (C.OpCode, Word8)
resolveVariable name = do
    maybeLocal <- resolveLocal name
    case maybeLocal of
        Just arg -> return (C.OpGetLocal, arg)
        Nothing -> resolveUpvalueOrGlobal name

resolveUpvalueOrGlobal :: S.Token -> CompilerM (C.OpCode, Word8)
resolveUpvalueOrGlobal name = do
    maybeUpvalue <- resolveUpvalue name
    case maybeUpvalue of
        Just arg -> return (C.OpGetUpvalue, arg)
        Nothing -> do
            arg <- identifierConstant name
            return (C.OpGetGlobal, arg)

and_ :: CompilerM ()
and_ = do
    endJump <- emitJump C.OpJumpIfFalse
    emitByte $ C.toByte C.OpPop
    parsePrecedence PrecAnd
    patchJump endJump

or_ :: CompilerM ()
or_ = do
    elseJump <- emitJump C.OpJumpIfFalse
    endJump <- emitJump C.OpJump
    patchJump elseJump
    emitByte $ C.toByte C.OpPop
    parsePrecedence PrecOr
    patchJump endJump

call :: CompilerM ()
call = do
    argCount <- argumentList
    emitBytes (C.toByte C.OpCall) argCount

argumentList :: CompilerM Word8
argumentList = do
    hasRightParen <- check S.TokenRightParen
    if hasRightParen
        then advance >> return 0
        else do
            expression
            count <- argumentListTail 1
            consume S.TokenRightParen "Expect ')' after arguments."
            return $ fromIntegral count

argumentListTail :: Int -> CompilerM Int
argumentListTail count = do
    hasComma <- match S.TokenComma
    if not hasComma
        then return count
        else
            when (count >= 255)
                (errorAtCurrent "Can't have more than 255 arguments.")
            >> expression >> argumentListTail (count + 1)

emitConstant :: Variable -> CompilerM ()
emitConstant var = do
    constant <- makeConstant var
    emitBytes (C.toByte C.OpConstant) constant

dot :: CompilerM ()
dot = do
    name <- parseIdentifier "Expect property name after '.'."
    constant <- identifierConstant name
    canAssign <- canAssignProperty
    hasEqual <- match S.TokenEqual
    if canAssign && hasEqual
        then setProperty constant
        else handlePropertyCall constant

handlePropertyCall :: Word8 -> CompilerM ()
handlePropertyCall constant = do
    hasCall <- match S.TokenLeftParen
    if hasCall
        then invokeProperty constant
        else getProperty constant

getProperty :: Word8 -> CompilerM ()
getProperty = emitBytes (C.toByte C.OpGetProperty)

canAssignProperty :: CompilerM Bool
canAssignProperty = do
    precLvl <- gets $ \s -> precedence (getRule $ S.tokenType $ currentToken s)
    return $ precLvl <= PrecAssignment

setProperty :: Word8 -> CompilerM ()
setProperty cst = expression >> emitBytes (C.toByte C.OpSetProperty) cst

invokeProperty :: Word8 -> CompilerM ()
invokeProperty constant = do
    argCount <- argumentList
    emitBytes (C.toByte C.OpInvoke) constant
    emitByte argCount

super_ :: CompilerM ()
super_ = do
    currentClass' <- gets currentClass
    checkSuperUsage currentClass'
    consume S.TokenDot "Expect '.' after 'super'."
    name <- parseIdentifier "Expect method name."
    constant <- identifierConstant name
    namedVariable (syntheticTokenStr "this") False
    hasCall <- match S.TokenLeftParen
    if hasCall then superInvoke constant else superGet constant

checkSuperUsage :: Maybe ClassCompiler -> CompilerM ()
checkSuperUsage Nothing =
    errorAtPrevious "Can't use 'super' outside of a class."
checkSuperUsage (Just cc) = unless (classHasSuperclass cc) $
    errorAtPrevious "Can't use 'super' in a class with no superclass."

superInvoke :: Word8 -> CompilerM ()
superInvoke constant = do
    argCount <- argumentList
    namedVariable (syntheticTokenStr "super") False
    emitBytes (C.toByte C.OpSuperInvoke) constant
    emitByte argCount

superGet :: Word8 -> CompilerM ()
superGet constant =
    namedVariable (syntheticTokenStr "super") False >>
    emitBytes (C.toByte C.OpGetSuper) constant

this_ :: CompilerM ()
this_ = do
    currentClass' <- gets currentClass
    case currentClass' of
        Nothing -> errorAtPrevious "Can't use 'this' outside of a class."
        Just _ -> namedVariable (syntheticTokenStr "this") False

syntheticToken :: T.Text -> S.Token
syntheticToken text = S.Token S.TokenIdentifier text 0 0

syntheticTokenStr :: String -> S.Token
syntheticTokenStr = syntheticToken . T.pack

modifyCurrentCompiler :: (Compiler -> Compiler) -> CompilerM ()
modifyCurrentCompiler f = modify $ \state ->
    state { current = f (current state) }

resolveUpvalue :: S.Token -> CompilerM (Maybe Word8)
resolveUpvalue name = do
    compiler <- gets current
    case compilerEnclosing compiler of
        Nothing -> return Nothing
        Just enclosing -> resolveInEnclosing name enclosing

resolveInEnclosing :: S.Token -> Compiler -> CompilerM (Maybe Word8)
resolveInEnclosing name enclosing = do
    local <- resolveLocalInEnclosing name enclosing
    case local of
        Just localIndex ->
            markCaptured localIndex enclosing >>
            addUpvalue localIndex True
        Nothing -> resolveUpvalueInEnclosing name enclosing >>= handleUpvalue

handleUpvalue :: Maybe Word8 -> CompilerM (Maybe Word8)
handleUpvalue (Just upvalueIdx) = addUpvalue upvalueIdx False
handleUpvalue Nothing = return Nothing

resolveLocalInEnclosing :: S.Token -> Compiler -> CompilerM (Maybe Word8)
resolveLocalInEnclosing = resolveLocalInCompiler

resolveLocalInCompiler :: S.Token -> Compiler -> CompilerM (Maybe Word8)
resolveLocalInCompiler token compiler =
    let locals = compilerLocals compiler
    in go (zip [(0::Int)..] locals)
    where
        go [] = return Nothing
        go ((i, local):rest) =
            if localName local == token
                then return $ Just $ fromIntegral i
                else go rest

resolveUpvalueInEnclosing :: S.Token -> Compiler -> CompilerM (Maybe Word8)
resolveUpvalueInEnclosing name enclosing =
    case compilerEnclosing enclosing of
        Nothing -> return Nothing
        Just _ -> resolveUpvalue name

addUpvalue :: Word8 -> Bool -> CompilerM (Maybe Word8)
addUpvalue index isLocal = do
    upvalues <- gets (compilerUpvalues . current)
    case findExistingUpvalue upvalues index isLocal of
        Just i -> return $ Just i
        Nothing -> addNewUpvalue upvalues index isLocal

addNewUpvalue :: [Upvalue] -> Word8 -> Bool -> CompilerM (Maybe Word8)
addNewUpvalue upvalues index isLocal =
    when (length upvalues >= 255)
        (errorAtPrevious "Too many closure variables in function.") >>
    let newUpvalue = Upvalue index isLocal
    in modifyCurrentCompiler
        (\c -> c { compilerUpvalues = compilerUpvalues c ++ [newUpvalue] }) >>
        return (Just $ fromIntegral (length upvalues))

findExistingUpvalue :: [Upvalue] -> Word8 -> Bool -> Maybe Word8
findExistingUpvalue upvalues index isLocal =
    findIndex (\up -> upvalueIndex up == index && upvalueIsLocal up == isLocal)
        upvalues
    >>= Just . fromIntegral

markCaptured :: Word8 -> Compiler -> CompilerM ()
markCaptured index enclosing =
    let locals = compilerLocals enclosing
    in when (index < fromIntegral (length locals)) $
        modifyCurrentCompiler $ \c -> c
            { compilerLocals = updateAt (fromIntegral index)
                (\local -> local { localIsCaptured = True })
                (compilerLocals c)
            }

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt _ _ [] = []
updateAt 0 f (x:xs) = f x : xs
updateAt n f (x:xs) = x : updateAt (n - 1) f xs

whenM :: Monad m => m Bool -> m () -> m ()
whenM cond action = do
    result <- cond
    when result action

synchronize :: CompilerM ()
synchronize = do
    _ <- modify (\s -> s { panicMode = False })
    advance
    while (not <$> atSyncPoint) advance
  where
    atSyncPoint = do
        currType <- gets (S.tokenType . currentToken)
        return $ currType `elem` [ S.TokenClass, S.TokenFun, S.TokenVar,
            S.TokenFor, S.TokenIf, S.TokenWhile, S.TokenPrint, S.TokenPrintLn,
            S.TokenSetRecursionLimit, S.TokenReturn, S.TokenEOF ]

forStatement :: CompilerM ()
forStatement =
    beginScope >>
    consume S.TokenLeftParen "Expect '(' after 'for'." >>
    handleForLoop >>
    endScope

handleForLoop :: CompilerM ()
handleForLoop = do
    unlessM (match S.TokenSemicolon) $
        ifM (match S.TokenVar) varDeclaration expressionStatement
    loopStart <- currentOffsetFor
    exitJump <- handleForCondition
    loopStart' <- handleForIncrement loopStart
    statement
    emitLoop loopStart'
    when (exitJump /= -1) $ patchJump exitJump >> emitByte (C.toByte C.OpPop)

handleForCondition :: CompilerM Int
handleForCondition = do
    hasCondition <- not <$> match S.TokenSemicolon
    if hasCondition
        then
            expression >>
            consume S.TokenSemicolon "Expect ';' after loop condition." >>
            emitConditionalJump
        else return (-1)

handleForIncrement :: Int -> CompilerM Int
handleForIncrement loopStart = do
    hasIncrement <- not <$> match S.TokenRightParen
    if hasIncrement
        then handleIncrementClause loopStart
        else return loopStart

handleIncrementClause :: Int -> CompilerM Int
handleIncrementClause loopStart = do
    bodyJump <- emitJump C.OpJump
    incrementStart <- currentOffsetFor
    expression
    emitByte $ C.toByte C.OpPop
    consume S.TokenRightParen "Expect ')' after for clauses."
    emitLoop loopStart
    patchJump bodyJump
    return incrementStart

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM cond action = do
    result <- cond
    unless result action

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond ifTrue ifFalse = do
    result <- cond
    if result then ifTrue else ifFalse

currentOffsetFor :: CompilerM Int
currentOffsetFor =
    gets (length . chunkCode . functionChunk . compilerFunction . current)

emitConditionalJump :: CompilerM Int
emitConditionalJump = do
    jump <- emitJump C.OpJumpIfFalse
    emitByte $ C.toByte C.OpPop
    return jump
