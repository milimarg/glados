{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- Serialize
-}

module Serialize (dumpFunction, loadFunction) where

import Types.Base ( Function )
import Codec.Compression.Lzma (compress, decompress)
import qualified Data.ByteString.Lazy as BL
import Data.Binary (encode, decode)

dumpFunction :: FilePath -> Function -> IO ()
dumpFunction filePath func = BL.writeFile filePath $ compress $ encode func

loadFunction :: FilePath -> IO Function
loadFunction filePath = do
    compressed <- BL.readFile filePath
    return $ decode $ decompress compressed
