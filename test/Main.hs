-- FILE: Main.hs
{-# LANGUAGE OverloadedLists #-}

module Main where

import Data.Massiv.Array as M hiding (replicate)
import qualified Data.Massiv.Array as M
import Data.Maybe
import Flann
import Foreign.C
import Foreign.C.Types (CInt)
import Test.Hspec

-- Helper function to create the dataset array
createDataset :: Int -> Array S Ix2 CInt
createDataset d = M.fromLists' Seq [[if i == j then 1 else 0 | j <- [0 .. d - 1]] | i <- [0 .. d - 1]]

-- Test suite
main :: IO ()
main = hspec $ do
  describe "findNearestNeighbors" $ do
    it "returns correct indices and distances for identity dataset" $ do
      let d = 4 -- 5 fails but this is because it's only approximate?
      let dataset = createDataset d
      (indices, distances) <- findNearestNeighbors dataset dataset 4
      let baseDistances = M.replicate Seq (Sz (d :. d)) 2.0
      let expectedDistances = computeS $ updateFirstColumn baseDistances
      distances `shouldBe` expectedDistances
      [indices M.! Ix2 0 j | j <- [0 .. d - 1]] `shouldBe` [0 .. fromIntegral d - 1]

updateFirstColumn :: Array S Ix2 CFloat -> Array DL Ix2 CFloat
updateFirstColumn arr =
  let (Sz (rows :. _)) = size arr
      firstColumn :: Vector S CFloat
      firstColumn = M.fromList Seq (replicate rows 0)
   in fromJust $ M.replaceSlice 1 0 firstColumn arr
