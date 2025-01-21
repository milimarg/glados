{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- Spec
-}

import Test.Hspec
import qualified ChunkSpec
import qualified CompilerSpec
import qualified Debug.UtilsSpec
import qualified Debug.DebugSpec
import qualified InputHandling.ArgsSpec
import qualified InputHandling.FileProcessorSpec
import qualified InputHandling.REPLSpec
import qualified PreprocessorSpec
import qualified ScannerSpec
import qualified SerializeSpec
import qualified Types.BaseSpec
import qualified UtilsSpec
import qualified VMSpec
import qualified Integrations.IntegrationsSpec

main :: IO ()
main = hspec $ do
    ChunkSpec.spec
    CompilerSpec.spec
    Debug.UtilsSpec.spec
    Debug.DebugSpec.spec
    InputHandling.ArgsSpec.spec
    InputHandling.FileProcessorSpec.spec
    InputHandling.REPLSpec.spec
    PreprocessorSpec.spec
    ScannerSpec.spec
    SerializeSpec.spec
    Types.BaseSpec.spec
    UtilsSpec.spec
    VMSpec.spec
    Integrations.IntegrationsSpec.spec
