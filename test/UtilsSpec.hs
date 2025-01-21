{-
-- EPITECH PROJECT, 2025
-- B-FUN-500-STG-5-2-glados-augustin.grosnon
-- File description:
-- UtilsSpec
-}

module UtilsSpec (
    spec
) where

import Test.Hspec
import Utils (safeIndex)

spec :: Spec
spec = describe "Utils" $ do
    it "should return Just the element at the given index" $ do
        safeIndex ([1, 2, 3] :: [Int]) 0 `shouldBe` Just 1
        safeIndex ([1, 2, 3] :: [Int]) 1 `shouldBe` Just 2
        safeIndex ([1, 2, 3] :: [Int]) 2 `shouldBe` Just 3

    it "should return Nothing for an out of bounds index" $ do
        safeIndex [1, 2, 3] 3 `shouldBe` (Nothing :: Maybe Int)
        safeIndex [1, 2, 3] (-1) `shouldBe` (Nothing :: Maybe Int)

    it "should return Nothing for an empty list" $ do
        safeIndex [] 0 `shouldBe` (Nothing :: Maybe Int)
        safeIndex [] 1 `shouldBe` (Nothing :: Maybe Int)
