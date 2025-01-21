{-
-- EPITECH PROJECT, 2024
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- Debug
-}

module Debug.Utils (
    debugLog,
    traceValue,
    traceFunction,
    tracePoint,
    traceAndReturn,
    traceList,
    traceMap,
    traceWhen,
    buildTraceMessage
) where

import Control.Monad (when)
import InputHandling.Args (Flags(..))
import Debug.Trace (trace)

debugLog :: Show a => Flags -> String -> a -> IO ()
debugLog Flags{debugMode = debug} msg value = when debug $
    putStrLn msg >>
    print value

traceValue :: Show a => String -> a -> a
traceValue label value = trace (label ++ ": " ++ show value) value

traceFunction :: (Show a, Show b) => String -> (a -> b) -> (a -> b)
traceFunction label f x =
    let result = f x
    in trace
        (label ++ " - Input: " ++ show x ++ ", Output: " ++ show result) result

tracePoint :: String -> a -> a
tracePoint message = trace ("Reached: " ++ message)

traceM :: (Show a, Monad m) => String -> a -> m ()
traceM label value = trace (label ++ ": " ++ show value) (return ())

traceAndReturn :: (Show a, Monad m) => String -> a -> m a
traceAndReturn label value = traceM label value >> return value

traceList :: Show a => String -> [a] -> [a]
traceList label = map (\x -> trace (label ++ ": " ++ show x) x)

traceMap :: (Show a, Show b) => String -> (a -> b) -> [a] -> [b]
traceMap label f = map (traceFunction label f)

traceWhen :: Show a => (a -> Bool) -> String -> a -> a
traceWhen condition label value =
    if condition value
        then trace (label ++ ": " ++ show value) value
        else value

buildTraceMessage :: Show a => String -> String -> Either String (a, String) -> String
buildTraceMessage label input result =
    case result of
        Left err             -> traceError label input err
        Right (value, rest)  -> traceSuccess label input value rest

traceSuccess :: Show a => String -> String -> a -> String -> String
traceSuccess label input value remaining =
    label ++ " - Input: " ++ show input ++
    ", Result: " ++ show value ++
    ", Remaining: " ++ show remaining

traceError :: String -> String -> String -> String
traceError label input errorMsg =
    label ++ " - Input: " ++ show input ++
    ", Error: " ++ errorMsg
