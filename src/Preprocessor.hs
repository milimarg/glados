{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- Preprocessor
-}

module Preprocessor (preprocess) where

import qualified Data.Text as T
import qualified Data.Map as Map

type PostProcessor = T.Text -> T.Text

preprocess :: T.Text -> T.Text
preprocess source =
    let (lines', macros) = processLines (T.lines source) defaultMacros
        processedText = replaceMacros macros lines'
    in applyPostProcessors postProcessors processedText

defaultMacros :: Map.Map T.Text T.Text
defaultMacros = Map.fromList
    [ (txt "++", txt "+= 1")
    , (txt "--", txt "-= 1")
    , (txt "||", txt "or")
    , (txt "&&", txt "and")
    , (txt "printuf", txt "print \"uf \"; println ")
    , (txt "printmiaou", txt "print \"miaou \"; println ")
    ]

postProcessors :: [PostProcessor]
postProcessors =
    [ replaceCompoundOperators,
      replaceInfixFunctions
    ]

processLines :: [T.Text] -> Map.Map T.Text T.Text -> ([T.Text], Map.Map T.Text T.Text)
processLines [] macros = ([], macros)
processLines (line:rest) macros
    | T.isPrefixOf (txt "#define ") line =
        let (key, value) = parseDefine (T.drop 8 line)
        in processLines rest (Map.insert key value macros)
    | otherwise =
        let (remainingLines, updatedMacros) = processLines rest macros
        in (line : remainingLines, updatedMacros)

parseDefine :: T.Text -> (T.Text, T.Text)
parseDefine line =
    let (key, rest) = T.breakOn (txt " ") line
    in (key, T.strip rest)

replaceMacros :: Map.Map T.Text T.Text -> [T.Text] -> T.Text
replaceMacros macros = T.unlines . map (replaceLine macros)

replaceLine :: Map.Map T.Text T.Text -> T.Text -> T.Text
replaceLine macros line = replaceOutsideStrings macros line False

replaceOutsideStrings :: Map.Map T.Text T.Text -> T.Text -> Bool -> T.Text
replaceOutsideStrings macros text inString = processText macros text inString 0

processText :: Map.Map T.Text T.Text -> T.Text -> Bool -> Int -> T.Text
processText macros remaining inString escapeCount =
    case T.uncons remaining of
        Nothing -> txt ""
        Just (c, rest) ->
            processChar macros c rest inString escapeCount

processChar :: Map.Map T.Text T.Text -> Char -> T.Text -> Bool -> Int -> T.Text
processChar macros c rest inString escapeCount
    | isUnescapedQuote c escapeCount = T.cons c
        (processText macros rest (not inString) 0)
    | inString = T.cons c
        (processText macros rest inString (updateEscapeCount c escapeCount))
    | otherwise = processOutsideString macros (T.cons c rest)

processOutsideString :: Map.Map T.Text T.Text -> T.Text -> T.Text
processOutsideString macros text =
    let (before, after) = splitTextAtQuote text
        replaced = applyMacros macros before
    in replaced <> processText macros after False 0

isUnescapedQuote :: Char -> Int -> Bool
isUnescapedQuote c escapeCount = c == '"' && even escapeCount

updateEscapeCount :: Char -> Int -> Int
updateEscapeCount c escapeCount = if c == '\\' then escapeCount + 1 else 0

splitTextAtQuote :: T.Text -> (T.Text, T.Text)
splitTextAtQuote = T.breakOn (txt "\"")

applyMacros :: Map.Map T.Text T.Text -> T.Text -> T.Text
applyMacros macros text = foldl (\acc (key, value) -> T.replace key value acc)
    text (Map.toList macros)

applyPostProcessors :: [PostProcessor] -> T.Text -> T.Text
applyPostProcessors processors text = foldl (\acc f -> f acc) text processors

replaceCompoundOperators :: PostProcessor
replaceCompoundOperators text =
    T.unlines $ map expandCompoundOperators (T.lines text)

expandCompoundOperators :: T.Text -> T.Text
expandCompoundOperators line =
    foldl (\acc (op, replacement) ->
        replaceOperator acc op replacement) line operators
  where
    operators =
        [ (txt "+=", txt "= "), (txt "-=", txt "= ")
        , (txt "*=", txt "= "), (txt "/=", txt "= ")
        , (txt "%=", txt "= ")
        ]

replaceOperator :: T.Text -> T.Text -> T.Text -> T.Text
replaceOperator line op replacement =
    if op `T.isInfixOf` line
    then
        let (before, afterOp) = T.breakOn op line
            var = detectVariableName before
            afterExpr = T.strip $ T.drop (T.length op) afterOp
        in T.concat [before, replacement, var,
                    txt " ", T.take 1 op, txt " ", afterExpr]
    else line

detectVariableName :: T.Text -> T.Text
detectVariableName text =
    case T.words text of
        [] -> txt ""
        tokens -> last tokens

txt :: String -> T.Text
txt = T.pack

replaceInfixFunctions :: PostProcessor
replaceInfixFunctions text =
    T.unlines $ map expandInfixFunctions (T.lines text)

expandInfixFunctions :: T.Text -> T.Text
expandInfixFunctions line = case T.breakOn (txt "`") line of
    (before, rest)
        | T.isInfixOf (txt "`") rest -> processInfixFunction before rest
        | otherwise -> line

processInfixFunction :: T.Text -> T.Text -> T.Text
processInfixFunction before rest =
    let (functionName, afterFunction) = extractFunctionName rest
        functionNameClean = T.strip functionName
        afterArgs = T.strip $ T.tail afterFunction
        (arg1, arg2, remaining, hasSemicolon) =
            extractArgumentsAndSemicolon (T.stripEnd before) afterArgs
        beforeCleaned = cleanBefore before arg1
    in formatInfixFunction beforeCleaned functionNameClean
        arg1 arg2 hasSemicolon remaining

extractFunctionName :: T.Text -> (T.Text, T.Text)
extractFunctionName rest = T.breakOn (txt "`") (T.tail rest)

cleanBefore :: T.Text -> T.Text -> T.Text
cleanBefore before arg1 =
    T.stripEnd $ T.dropEnd (T.length arg1) (T.strip before)

formatInfixFunction :: T.Text -> T.Text -> T.Text -> T.Text -> Bool -> T.Text -> T.Text
formatInfixFunction before functionNameClean arg1 arg2 hasSemicolon remaining =
    T.concat
        [ before, txt " ", functionNameClean
        , txt "(", arg1, txt ", ", arg2, txt ")"
        , if hasSemicolon then txt ";" else txt "", remaining
        ]

extractArgumentsAndSemicolon :: T.Text -> T.Text -> (T.Text, T.Text, T.Text, Bool)
extractArgumentsAndSemicolon before after =
    let arg1 = extractArg1 before
        afterStripped = T.strip after
        (arg2, rest) = extractArg2 afterStripped
        hasSemicolon = T.isPrefixOf (txt ";") rest
        remaining = dropSpaceOrSemicolon rest
    in (arg1, arg2, remaining, hasSemicolon)

extractArg1 :: T.Text -> T.Text
extractArg1 = T.takeWhileEnd (not . isArgumentStopChar)

extractArg2 :: T.Text -> (T.Text, T.Text)
extractArg2 = T.break isArgumentStopChar

dropSpaceOrSemicolon :: T.Text -> T.Text
dropSpaceOrSemicolon = T.dropWhile isSpaceOrSemicolon

isArgumentStopChar :: Char -> Bool
isArgumentStopChar c = isSpace c || c `elem` "();{}[]"

isSpaceOrSemicolon :: Char -> Bool
isSpaceOrSemicolon c = isSpace c || c == ';'

isSpace :: Char -> Bool
isSpace = (`elem` [' ', '\t', '\n'])
