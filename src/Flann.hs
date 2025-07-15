{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Flann
  ( module Flann,

    -- * types
    module F,
  )
where

import Data.Massiv.Array
import Flann.Internal as F (DT, FlannAlgorithmT (..), FlannCentersInitT (..), FlannLogLevelT, LoadIndex (..), SaveIndex (..), type BuildIndex, type ComputeClusterCenters, type FindNearestNeighbors, type FindNearestNeighborsIndex, type FlannIndexT)
import qualified Flann.Internal as FI
import Foreign
import Foreign.C

findNearestNeighbors ::
  (_) =>
  -- | @dataset(n,m)@
  Array S Ix2 t ->
  -- | @testset(q,m)@
  Array S Ix2 t ->
  -- | @k@ number of neighbors
  Int ->
  -- | @indexes(q,k), dist(q,k)@
  IO (Array S Ix2 CInt, Array S Ix2 (DT t))
findNearestNeighbors dataset testset nn = do
  FI.findNearestNeighbors dataset testset nn =<< defParams

-- | see also 'saveIndex' 'loadIndex'
buildIndex ::
  (_) =>
  -- | dataset(n,m)
  Array S Ix2 t ->
  -- | index
  IO (FlannIndexT t)
buildIndex dataset = do
  fmap fst . FI.buildIndex dataset =<< defParams

computeClusterCenters ::
  (_) =>
  -- | @dataset(n,m)@
  Array S Ix2 t ->
  -- | @k@ number of clusters
  Int ->
  -- | @cluster_centers(k,m)@
  IO (Array S Ix2 (DT t))
computeClusterCenters dataset numClusters = do
  fmap computeS . FI.computeClusterCenters dataset numClusters =<< defParams

findNearestNeighborsIndex ::
  (_) =>
  -- | @index@ from 'buildIndex'
  FlannIndexT t ->
  -- | @testset(q,m)@
  Array S Ix2 t ->
  -- | @k@ number of neighbors
  Int ->
  -- | @indexes(q,nn), dist(q,nn)@
  IO (Array S Ix2 CInt, Array S Ix2 (DT t))
findNearestNeighborsIndex index testset nn = do
  FI.findNearestNeighborsIndex index testset nn =<< defParams

-- * read the global FLANNParameters

get_branching :: IO CInt
get_branching = FI.get_branching =<< defParams

get_iterations :: IO CInt
get_iterations = FI.get_iterations =<< defParams

get_algorithm :: IO FlannAlgorithmT
get_algorithm = FI.get_algorithm =<< defParams

get_centers_init :: IO FlannCentersInitT
get_centers_init = FI.get_centers_init =<< defParams

get_eps :: IO CFloat
get_eps = FI.get_eps =<< defParams

get_target_precision :: IO CFloat
get_target_precision = FI.get_target_precision =<< defParams

get_build_weight :: IO CFloat
get_build_weight = FI.get_build_weight =<< defParams

get_memory_weight :: IO CFloat
get_memory_weight = FI.get_memory_weight =<< defParams

get_sample_fraction :: IO CFloat
get_sample_fraction = FI.get_sample_fraction =<< defParams

get_table_number_ :: IO CUInt
get_table_number_ = FI.get_table_number_ =<< defParams

get_key_size_ :: IO CUInt
get_key_size_ = FI.get_key_size_ =<< defParams

get_multi_probe_level_ :: IO CUInt
get_multi_probe_level_ = FI.get_multi_probe_level_ =<< defParams

get_log_level :: IO FlannLogLevelT
get_log_level = FI.get_log_level =<< defParams

get_random_seed :: IO CLong
get_random_seed = FI.get_random_seed =<< defParams

-- * write the global FLANNParameters

set_algorithm :: FlannAlgorithmT -> IO ()
set_algorithm a = FI.set_algorithm a =<< defParams

set_checks :: CInt -> IO ()
set_checks c = FI.set_checks c =<< defParams

set_cb_index :: CFloat -> IO ()
set_cb_index cb = FI.set_cb_index cb =<< defParams

set_trees :: CInt -> IO ()
set_trees t = FI.set_trees t =<< defParams

set_leaf_max_size :: CInt -> IO ()
set_leaf_max_size l = FI.set_leaf_max_size l =<< defParams

set_branching :: CInt -> IO ()
set_branching b = FI.set_branching b =<< defParams

set_iterations :: CInt -> IO ()
set_iterations i = FI.set_iterations i =<< defParams

set_centers_init :: FlannCentersInitT -> IO ()
set_centers_init c = FI.set_centers_init c =<< defParams

set_eps :: CFloat -> IO ()
set_eps e = FI.set_eps e =<< defParams

set_target_precision :: CFloat -> IO ()
set_target_precision tp = FI.set_target_precision tp =<< defParams

set_build_weight :: CFloat -> IO ()
set_build_weight bw = FI.set_build_weight bw =<< defParams

set_memory_weight :: CFloat -> IO ()
set_memory_weight mw = FI.set_memory_weight mw =<< defParams

set_sample_fraction :: CFloat -> IO ()
set_sample_fraction sf = FI.set_sample_fraction sf =<< defParams

set_table_number_ :: CUInt -> IO ()
set_table_number_ tn = FI.set_table_number_ tn =<< defParams

set_key_size_ :: CUInt -> IO ()
set_key_size_ ks = FI.set_key_size_ ks =<< defParams

set_multi_probe_level_ :: CUInt -> IO ()
set_multi_probe_level_ mpl = FI.set_multi_probe_level_ mpl =<< defParams

set_log_level :: FlannLogLevelT -> IO ()
set_log_level ll = FI.set_log_level ll =<< defParams

set_random_seed :: CLong -> IO ()
set_random_seed rs = FI.set_random_seed rs =<< defParams

defParams = FI.FLANNParameters <$> newForeignPtr_ FI.defaultFlannParameters_
