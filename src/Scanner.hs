{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- Scanner
-}

module Scanner (
    Scanner(..),
    TokenType(..),
    Token(..),
    initScanner,
    scanToken
) where

import qualified Data.Text as T
import Data.Char (isAlpha, isDigit, isAlphaNum)
import Control.Monad.State (State, state, get, modify, gets)
import qualified Data.Map as M

data TokenType
    = TokenLeftParen | TokenRightParen
    | TokenLeftBrace | TokenRightBrace
    | TokenLeftSqBrace | TokenRightSqBrace
    | TokenComma | TokenDot | TokenMinus | TokenPlus | TokenModulo
    | TokenSemicolon | TokenSlash | TokenStar
    | TokenQuestionMark | TokenColon | TokenBang | TokenBangEqual
    | TokenEqual | TokenEqualEqual
    | TokenGreater | TokenGreaterEqual
    | TokenLess | TokenLessEqual | TokenPrint | TokenPrintLn
    | TokenIdentifier | TokenString | TokenNumber
    | TokenAnd | TokenClass | TokenElse | TokenFalse
    | TokenFor | TokenFun | TokenIf | TokenTernary | TokenNil | TokenOr
    | TokenSetRecursionLimit | TokenReturn | TokenSuper | TokenThis
    | TokenTrue | TokenVar | TokenList | TokenTuple | TokenMap | TokenSafeMap
    | TokenWhile | TokenError | TokenEOF
    deriving (Show, Eq)

data Token = Token
    { tokenType :: TokenType
    , tokenLexeme :: T.Text
    , tokenLine :: Int
    , tokenColumn :: Int
    } deriving (Show, Eq)

data Scanner = Scanner
    { source :: T.Text
    , start :: Int
    , current :: Int
    , line :: Int
    , column :: Int
    } deriving (Show, Eq)

initScanner :: T.Text -> Scanner
initScanner src = Scanner
    { source = src
    , start = 0
    , current = 0
    , line = 1
    , column = 1
    }

isAtEnd :: Scanner -> Bool
isAtEnd s = current s >= T.length (source s)

peek :: Scanner -> Maybe Char
peek s = if isAtEnd s
            then Nothing
            else Just $ T.index (source s) (current s)

peekNext :: Scanner -> Maybe Char
peekNext s = if current s + 1 >= T.length (source s)
                then Nothing
                else Just $ T.index (source s) (current s + 1)

advance :: State Scanner Char
advance = state $ \s ->
    let c = T.index (source s) (current s)
        ns = if c == '\n'
            then s { current = current s + 1, column = 1, line = line s + 1 }
            else s { current = current s + 1, column = column s + 1 }
    in (c, ns)

match :: Char -> State Scanner Bool
match expected = do
    s <- get
    if isAtEnd s || T.index (source s) (current s) /= expected
        then return False
        else advance >> return True

makeToken :: TokenType -> State Scanner Token
makeToken type_ = state $ \s ->
        let lexeme = T.take (current s - start s) $ T.drop (start s) $ source s
            startColumn = column s - T.length lexeme
        in (Token type_ lexeme (line s) startColumn, s { start = current s })

errorToken :: T.Text -> State Scanner Token
errorToken msg = state $ \s -> (Token TokenError msg (line s) (column s), s)

skipWhitespace :: State Scanner ()
skipWhitespace = do
    s <- get
    case peek s of
        Just c | c `elem` " \r\t\n" -> advance >> skipWhitespace
               | c == '/' -> handleSlash
               | otherwise -> return ()
        Nothing -> return ()

handleSlash :: State Scanner ()
handleSlash = do
    s <- get
    case peekNext s of
        Just '/' -> skipLine >> skipWhitespace
        Just '*' -> skipMultilineComment >> skipWhitespace
        _ -> return ()

skipMultilineComment :: State Scanner ()
skipMultilineComment = do
    s <- get
    case peek s of
        Just '*' -> checkClosingComment
        Just _   -> advance >> skipMultilineComment
        Nothing  -> return ()

checkClosingComment :: State Scanner ()
checkClosingComment = do
    s <- get
    case peekNext s of
        Just '/' -> advance >> advance >> return ()
        _ -> advance >> skipMultilineComment

skipLine :: State Scanner ()
skipLine = do
    s <- get
    case peek s of
        Just '\n' -> return ()
        Just _ -> advance >> skipLine
        Nothing -> return ()

checkKeyword :: T.Text -> TokenType
checkKeyword text =
    M.findWithDefault TokenIdentifier (T.unpack text) keywordMap

keywordMap :: M.Map String TokenType
keywordMap = M.fromList
    [ ("and", TokenAnd), ("or", TokenOr), ("class", TokenClass)
    , ("false", TokenFalse), ("true", TokenTrue), ("nil", TokenNil)
    , ("if", TokenIf), ("else", TokenElse), ("println", TokenPrintLn)
    , ("print", TokenPrint), ("return", TokenReturn)
    , ("setRecursionLimit", TokenSetRecursionLimit), ("while", TokenWhile)
    , ("fun", TokenFun), ("super", TokenSuper), ("this", TokenThis)
    , ("list", TokenList), ("tuple", TokenTuple), ("map", TokenMap)
    , ("safemap", TokenSafeMap), ("var", TokenVar) , ("for", TokenFor)
    ]

scanToken :: State Scanner Token
scanToken = do
    skipWhitespace
    updateStartPosition
    s <- get
    if isAtEnd s
        then makeToken TokenEOF
        else do
            c <- advance
            processChar c

updateStartPosition :: State Scanner ()
updateStartPosition = modify $ \s -> s { start = current s }

processChar :: Char -> State Scanner Token
processChar '(' = makeToken TokenLeftParen
processChar ')' = makeToken TokenRightParen
processChar '[' = makeToken TokenLeftSqBrace
processChar ']' = makeToken TokenRightSqBrace
processChar '{' = makeToken TokenLeftBrace
processChar '}' = makeToken TokenRightBrace
processChar ';' = makeToken TokenSemicolon
processChar ',' = makeToken TokenComma
processChar '.' = makeToken TokenDot
processChar '-' = makeToken TokenMinus
processChar '+' = makeToken TokenPlus
processChar '/' = makeToken TokenSlash
processChar '*' = makeToken TokenStar
processChar '%' = makeToken TokenModulo
processChar '?' = makeToken TokenQuestionMark
processChar ':' = makeToken TokenColon
processChar '!' = handleBang
processChar '=' = handleEqual
processChar '<' = handleLessThan
processChar '>' = handleGreaterThan
processChar '"' = scanString
processChar '_' = scanIdentifier
processChar ch
    | isDigit ch = scanNumber
    | isAlpha ch = scanIdentifier
    | otherwise = errorToken $ T.pack "Unexpected character."

handleBang :: State Scanner Token
handleBang = do
    matched <- match '='
    makeToken $ if matched then TokenBangEqual else TokenBang

handleEqual :: State Scanner Token
handleEqual = do
    matched <- match '='
    makeToken $ if matched then TokenEqualEqual else TokenEqual

handleLessThan :: State Scanner Token
handleLessThan = do
    dashMatch <- match '-'
    if dashMatch
        then handleTernary
        else handleEqualMatchInLess

handleEqualMatchInLess :: State Scanner Token
handleEqualMatchInLess = do
    equalMatch <- match '='
    let token = if equalMatch then TokenLessEqual else TokenLess
    makeToken token

handleTernary :: State Scanner Token
handleTernary = do
    matched <- match '>'
    if matched
        then makeToken TokenTernary
        else errorToken $
            T.pack "Unexpected character after '-' in ternary operator."

handleGreaterThan :: State Scanner Token
handleGreaterThan = do
    matched <- match '='
    makeToken $ if matched then TokenGreaterEqual else TokenGreater

scanString :: State Scanner Token
scanString = handlePeek =<< gets peek

handlePeek :: Maybe Char -> State Scanner Token
handlePeek (Just '"') = closeString
handlePeek (Just '\n') = advanceLine >> scanString
handlePeek (Just _) = advance >> scanString
handlePeek Nothing = errorToken $ T.pack "Unterminated string."

closeString :: State Scanner Token
closeString = do
    s <- get
    _ <- advance
    let lexeme = extractLexeme s
    return $ Token TokenString lexeme (line s) (column s - T.length lexeme - 2)

advanceLine :: State Scanner ()
advanceLine = modify $ \s -> s { line = line s + 1, column = 1 }

extractLexeme :: Scanner -> T.Text
extractLexeme s = T.take (current s - start s - 1) $
    T.drop (start s + 1) $ source s

scanNumber :: State Scanner Token
scanNumber = scanDigits >> handleFraction

handleFraction :: State Scanner Token
handleFraction = do
    s <- get
    case (peek s, peekNext s) of
        (Just '.', Just d) | isDigit d -> advance
            >> scanDigits >> makeToken TokenNumber
        _ -> makeToken TokenNumber

scanDigits :: State Scanner ()
scanDigits = do
    s <- get
    case peek s of
        Just d | isDigit d -> do
            _ <- advance
            scanDigits
        _ -> return ()

scanIdentifier :: State Scanner Token
scanIdentifier = do
    scanIdentifierChars
    s <- get
    let lexeme = T.take (current s - start s) $ T.drop (start s) $ source s
    let type_ = checkKeyword lexeme
    let startColumn = column s - T.length lexeme
    return $ Token type_ lexeme (line s) startColumn

scanIdentifierChars :: State Scanner ()
scanIdentifierChars = do
    s <- get
    case peek s of
        Just c | isAlphaNum c || c == '_' -> do
            _ <- advance
            scanIdentifierChars
        _ -> return ()
