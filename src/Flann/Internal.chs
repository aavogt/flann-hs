{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternGuards #-}
module Flann.Internal where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Control.Applicative
import qualified Data.Massiv.Array as M
import Data.Massiv.Array hiding (takeWhile)
import Data.Massiv.Array.Unsafe (unsafeWithPtr)
import Foreign.ForeignPtr
import System.IO.Unsafe
import Foreign.Marshal.Utils
import Foreign.Ptr
import Control.Monad

import C2HS hiding (cToEnum, cFromEnum, unsafePerformIO)


#include "flann_adapter.h"

-- * Find neighbors without a separate index

-- | flann works with points represented as arrays of either CFloat, CDouble, CUChar or CInt,
-- (float, double, unsigned char, int suffixes respectively)
-- but the distances or cluster centers are either float or double, as encoded by 'DT'
type family DT a where
  DT CDouble = CDouble
  DT a = CFloat

class FindNearestNeighbors t where
  findNearestNeighbors ::
        Array S Ix2 t -- ^ @dataset@ m-by-n array of m n-dimensional points
     -> Array S Ix2 t -- ^ @testset@ q-by-n array of q n-dimensional points whose nearest neighbours are to be found
     -> Int            -- ^ @nn@ number of neighbours to be found for each query point
     -> FLANNParameters -- ^ 'newFlannParameters'
     -> IO (Array S Ix2 CInt, Array S Ix2 (DT t))
            {- ^   @indices@ q-by-nn matrix whose entries are 0-based indexes into
                   points (rows) of the dataset.

                   @dists@ q--by-nn matrix of distances

                   > dists[i,j] = testset[i, :] `distance` dataset[indices[i, j], :]-}

-- | 'flannFindNearestNeighborsDouble'
instance FindNearestNeighbors CDouble where findNearestNeighbors = makeFindNearestNeighborsWrapper flannFindNearestNeighborsDouble
-- | 'flannFindNearestNeighborsFloat'
instance FindNearestNeighbors CFloat  where findNearestNeighbors = makeFindNearestNeighborsWrapper flannFindNearestNeighborsFloat
-- | 'flannFindNearestNeighborsByte'
instance FindNearestNeighbors CUChar  where findNearestNeighbors = makeFindNearestNeighborsWrapper flannFindNearestNeighborsByte
-- | 'flannFindNearestNeighborsInt'
instance FindNearestNeighbors CInt    where findNearestNeighbors = makeFindNearestNeighborsWrapper flannFindNearestNeighborsInt

class ComputeClusterCenters t where
    computeClusterCenters
        :: Array S Ix2 t
        -> Int -- ^ number of clusters
        -> FLANNParameters
        -> IO (Array D Ix2 (DT t))


-- | 'flannComputeClusterCentersDouble'
instance ComputeClusterCenters CDouble where computeClusterCenters = makeComputeClusterCenters flannComputeClusterCentersDouble
-- | 'flannComputeClusterCentersFloat'
instance ComputeClusterCenters CFloat  where computeClusterCenters = makeComputeClusterCenters flannComputeClusterCentersFloat
-- | 'flannComputeClusterCentersByte'
instance ComputeClusterCenters CUChar  where computeClusterCenters = makeComputeClusterCenters flannComputeClusterCentersByte
-- | 'flannComputeClusterCentersInt'
instance ComputeClusterCenters CInt    where computeClusterCenters = makeComputeClusterCenters flannComputeClusterCentersInt


-- * Find neighbors with an index
class BuildIndex t where
    buildIndex :: Array S Ix2 t -- ^ n-by-m matrix containing n m-dimensional points
                -> FLANNParameters
                ->IO (FlannIndexT t, CFloat) -- ^ ( index , speedup)

-- | 'flann_free_index_double2'
instance BuildIndex CDouble where buildIndex = makeBuildIndex flannBuildIndexDouble flann_free_index_double2
-- | 'flann_free_index_float2'
instance BuildIndex CFloat  where buildIndex = makeBuildIndex flannBuildIndexFloat flann_free_index_float2
-- | 'flann_free_index_byte2'
instance BuildIndex CUChar  where buildIndex = makeBuildIndex flannBuildIndexByte  flann_free_index_byte2
-- | 'flann_free_index_int2'
instance BuildIndex CInt    where buildIndex = makeBuildIndex flannBuildIndexInt flann_free_index_int2


class FindNearestNeighborsIndex t where
    findNearestNeighborsIndex :: FlannIndexT t -- ^ @buildIndex dataset@ with dataset m-by-n
      -> Array S Ix2 t -- ^ @testset@ q-by-n
      -> Int -- ^ nn
      -> FLANNParameters
      -> IO (Array S Ix2 CInt, Array S Ix2 (DT t))

-- | 'flannFindNearestNeighborsIndexDouble'
instance FindNearestNeighborsIndex CDouble where findNearestNeighborsIndex = makeFindNearestNeighborsIndex flannFindNearestNeighborsIndexDouble
-- | 'flannFindNearestNeighborsIndexFloat'
instance FindNearestNeighborsIndex CFloat where findNearestNeighborsIndex = makeFindNearestNeighborsIndex flannFindNearestNeighborsIndexFloat
-- | 'flannFindNearestNeighborsIndexInt'
instance FindNearestNeighborsIndex CInt where findNearestNeighborsIndex = makeFindNearestNeighborsIndex flannFindNearestNeighborsIndexInt
-- | 'flannFindNearestNeighborsIndexByte'
instance FindNearestNeighborsIndex CUChar where findNearestNeighborsIndex = makeFindNearestNeighborsIndex flannFindNearestNeighborsIndexByte


class RadiusSearch t where
    radiusSearch :: FlannIndexT t
        -> Array S Ix2 t -- ^ point whose neighbours are to be found
        -> Int -- ^ maximum of nearest neighbours
        -> Float -- ^ distance or maximum radius^2 (euclidean norm)
        -> FLANNParameters
        -> IO (Array S Ix2 CInt, Array S Ix2 (DT t))
            -- ^ indexes and distances of the neighbours found

-- | 'flannRadiusSearchDouble'
instance RadiusSearch CDouble where radiusSearch = makeRadiusSearch flannRadiusSearchDouble
-- | 'flannRadiusSearchFloat'
instance RadiusSearch CFloat where radiusSearch = makeRadiusSearch flannRadiusSearchFloat
-- | 'flannRadiusSearchByte'
instance RadiusSearch CUChar where radiusSearch = makeRadiusSearch flannRadiusSearchByte
-- | 'flannRadiusSearchInt'
instance RadiusSearch CInt where radiusSearch = makeRadiusSearch flannRadiusSearchInt


-- ** serialize the index
class SaveIndex t where
    saveIndex :: FlannIndexT t -> FilePath -> IO ()

-- | 'flannSaveIndexDouble'
instance SaveIndex CDouble where saveIndex index filename = checkSaveIndex (flannSaveIndexDouble index filename)

-- | 'flannSaveIndexFloat'
instance SaveIndex CFloat where saveIndex index filename = checkSaveIndex (flannSaveIndexFloat index filename)

-- | 'flannSaveIndexByte'
instance SaveIndex CUChar where saveIndex index filename = checkSaveIndex (flannSaveIndexByte index filename)

-- | 'flannSaveIndexInt'
instance SaveIndex CInt where saveIndex index filename = checkSaveIndex (flannSaveIndexInt index filename)

class LoadIndex t where
  loadIndex :: FilePath -> Array S Ix2 t -- ^ @dataset@ used in the 'buildIndex' call
    -> IO (FlannIndexT t)

-- | 'flannLoadIndexDouble'
instance LoadIndex CDouble where loadIndex = makeLoadIndex flann_free_index_double flannLoadIndexDouble
-- | 'flannLoadIndexFloat'
instance LoadIndex CFloat  where loadIndex = makeLoadIndex flann_free_index_float flannLoadIndexFloat
-- | 'flannLoadIndexByte'
instance LoadIndex CUChar  where loadIndex = makeLoadIndex flann_free_index_byte flannLoadIndexByte
-- | 'flannLoadIndexInt'
instance LoadIndex CInt    where loadIndex = makeLoadIndex flann_free_index_int flannLoadIndexInt

-- ** add/remove points from the index

class ModifyIndex t where
  addPoints :: FlannIndexT t -> Array S Ix2 t -> IO Int
  removePoint :: FlannIndexT t -> CUInt -> IO Int

-- | 'flannAddPointsFloat', 'flannRemovePointFloat'
instance ModifyIndex CFloat where
  addPoints index points =
    let Sz2 n m = M.size points
    in flannAddPointsFloat index points n m 0.8
  removePoint = flannRemovePointFloat

instance ModifyIndex CDouble where
  addPoints index points =
    let Sz2 n m = M.size points
    in flannAddPointsDouble index points n m 0.8
  removePoint = flannRemovePointDouble

instance ModifyIndex CUChar where
  addPoints index points =
    let Sz2 n m = M.size points
    in flannAddPointsByte index points n m 0.8
  removePoint = flannRemovePointByte

instance ModifyIndex CInt where
  addPoints index points =
    let Sz2 n m = M.size points
    in flannAddPointsInt index points n m 0.8
  removePoint = flannRemovePointInt

-- * @FLANNParameters@
{- | This should be treated as an opaque object.

It should arguably contain the 'flannSetDT', which could be faked in
the haskell binding.
-}
newtype FLANNParameters = FLANNParameters { unFLANNParameters :: ForeignPtr () }

-- ** IO getters
get_algorithm x = fmap (\x -> cToEnum x :: FlannAlgorithmT) $ withFLANNParameters x {# get FLANNParameters.algorithm #}
get_checks x = withFLANNParameters x {# get FLANNParameters.checks #}
get_cb_index x = withFLANNParameters x {# get FLANNParameters.cb_index #}
get_trees x = withFLANNParameters x {# get FLANNParameters.trees #}
get_leaf_max_size x = withFLANNParameters x {# get FLANNParameters.leaf_max_size #}
get_branching x = withFLANNParameters x {# get FLANNParameters.branching #}
get_iterations x = withFLANNParameters x {# get FLANNParameters.iterations #}
get_centers_init x = fmap (\x -> cToEnum x :: FlannCentersInitT)
            $ withFLANNParameters x {# get FLANNParameters.centers_init #}

get_eps x = withFLANNParameters x {# get FLANNParameters.eps #}
get_target_precision x = withFLANNParameters x {# get FLANNParameters.target_precision #}
get_build_weight x = withFLANNParameters x {# get FLANNParameters.build_weight #}
get_memory_weight x = withFLANNParameters x {# get FLANNParameters.memory_weight #}
get_sample_fraction x = withFLANNParameters x {# get FLANNParameters.sample_fraction #}
get_table_number_ x = withFLANNParameters x {# get FLANNParameters.table_number_ #}
get_key_size_ x = withFLANNParameters x {# get FLANNParameters.key_size_ #}
get_multi_probe_level_ x = withFLANNParameters x {# get FLANNParameters.multi_probe_level_ #}

get_log_level x = fmap (\x -> cToEnum x :: FlannLogLevelT)
                $ withFLANNParameters x {# get FLANNParameters.log_level #}
get_random_seed x = withFLANNParameters x {# get FLANNParameters.random_seed #}

-- ** IO setters
{#fun flann_set_distance_type as ^ { cFromEnum `FlannDistanceT', `Int' } -> `()' #}
set_algorithm n x = withFLANNParameters x $ \x -> {# set FLANNParameters.algorithm #} x (cFromEnum (n:: FlannAlgorithmT))
set_checks n x = withFLANNParameters x $ \x -> {# set FLANNParameters.checks #} x n
set_cb_index n x = withFLANNParameters x $ \x -> {# set FLANNParameters.cb_index #} x n
set_trees n x = withFLANNParameters x $ \x -> {# set FLANNParameters.trees #} x n
set_leaf_max_size n x = withFLANNParameters x $ \x -> {# set FLANNParameters.leaf_max_size #} x n
set_branching n x = withFLANNParameters x $ \x -> {# set FLANNParameters.branching #} x n
set_iterations n x = withFLANNParameters x $ \x -> {# set FLANNParameters.iterations #} x n
set_centers_init n x = withFLANNParameters x $ \x -> {# set FLANNParameters.centers_init #} x (cFromEnum (n:: FlannCentersInitT))
set_eps n x = withFLANNParameters x $ \x -> {# set FLANNParameters.eps #} x n

set_target_precision n x = withFLANNParameters x $ \x -> {# set FLANNParameters.target_precision #} x n
set_build_weight n x = withFLANNParameters x $ \x -> {# set FLANNParameters.build_weight #} x n
set_memory_weight n x = withFLANNParameters x $ \x -> {# set FLANNParameters.memory_weight #} x n
set_sample_fraction n x = withFLANNParameters x $ \x -> {# set FLANNParameters.sample_fraction #} x n
set_table_number_ n x = withFLANNParameters x $ \x -> {# set FLANNParameters.table_number_ #} x n
set_key_size_ n x = withFLANNParameters x $ \x -> {# set FLANNParameters.key_size_ #} x n
set_multi_probe_level_ n x = withFLANNParameters x $ \x -> {# set FLANNParameters.multi_probe_level_ #} x n

set_log_level n x = withFLANNParameters x $ \x -> {# set FLANNParameters.log_level #} x (cFromEnum (n:: FlannLogLevelT))
set_random_seed n x = withFLANNParameters x $ \x -> {# set FLANNParameters.random_seed #} x n




-- ** Creating @FLANNParameters@

-- | default parameters
newFlannParameters :: IO FLANNParameters
newFlannParameters = do
    let n = {# sizeof FLANNParametersStruct #}
    y <- mallocForeignPtrBytes n
    withForeignPtr y $ \to -> copyBytes to defaultFlannParameters_ n
    return (FLANNParameters y)

copyFlannParameters :: FLANNParameters -> IO FLANNParameters
copyFlannParameters (FLANNParameters from) = do
    let n = {# sizeof FLANNParametersStruct #}
    y <- mallocForeignPtrBytes n
    withForeignPtr from $ \from -> withForeignPtr y $ \to -> copyBytes to from n
    return (FLANNParameters y)


-- * Probably not necessary to use (ie. private)


-- ** Find nearest neighbour
{#fun flann_find_nearest_neighbors_double as ^
    { unsafeWithPtr* `M.Array S Ix2 CDouble',
      `Int',
      `Int',
      unsafeWithPtr* `M.Array S Ix2 CDouble',
      `Int',
      unsafeWithPtr* `M.Array S Ix2 CInt',
      unsafeWithPtr* `M.Array S Ix2 CDouble',
      `Int',
      withFLANNParameters* `FLANNParameters'} -> `CInt' id #}

{#fun flann_find_nearest_neighbors_float as ^
    { unsafeWithPtr* `M.Array S Ix2 CFloat',
      `Int', `Int',
      unsafeWithPtr* `M.Array S Ix2 CFloat',
      `Int',
      unsafeWithPtr* `M.Array S Ix2 CInt',
      unsafeWithPtr* `M.Array S Ix2 CFloat',
      `Int',
      withFLANNParameters* `FLANNParameters'} -> `CInt' id #}

{#fun flann_find_nearest_neighbors_byte as ^
    { unsafeWithPtr* `M.Array S Ix2 CUChar',
      `Int',
      `Int',
      unsafeWithPtr* `M.Array S Ix2 CUChar',
      `Int',
      unsafeWithPtr* `M.Array S Ix2 CInt',
      unsafeWithPtr* `M.Array S Ix2 CFloat',
      `Int',
      withFLANNParameters* `FLANNParameters'} -> `CInt' id #}

{#fun flann_find_nearest_neighbors_int as ^
    { unsafeWithPtr* `M.Array S Ix2 CInt',
      `Int',
      `Int',
      unsafeWithPtr* `M.Array S Ix2 CInt',
      `Int',
      unsafeWithPtr* `M.Array S Ix2 CInt',
      unsafeWithPtr* `M.Array S Ix2 CFloat',
      `Int',
      withFLANNParameters* `FLANNParameters'} -> `CInt' id #}

-- ** Creating an index

{- | the type @a@ represents the type of point used to create the index

-}
newtype FlannIndexT a = FlannIndexT (ForeignPtr ())
withFlannIndexT (FlannIndexT x) = withForeignPtr x


{#fun flann_build_index_double as ^
        { unsafeWithPtr* `M.Array S Ix2 CDouble',
        `Int', `Int', alloca- `CFloat' peek*,
        withFLANNParameters* `FLANNParameters' } -> `(Ptr ())' id  #}


{#fun flann_build_index_float as ^
     { unsafeWithPtr* `M.Array S Ix2 CFloat',
       `Int', `Int', alloca- `CFloat' peek*,
       withFLANNParameters* `FLANNParameters' } -> `Ptr ()' id #}

{#fun flann_build_index_byte as ^
     { unsafeWithPtr* `M.Array S Ix2 CUChar',
       `Int', `Int', alloca- `CFloat' peek*,
       withFLANNParameters* `FLANNParameters' } -> `Ptr ()' id #}

{#fun flann_build_index_int as ^
     { unsafeWithPtr* `M.Array S Ix2 CInt',
       `Int', `Int', alloca- `CFloat' peek*,
       withFLANNParameters* `FLANNParameters' } -> `Ptr ()' id #}

-- ** add points
{#fun flann_add_points_float as ^
    { withFlannIndexT* `FlannIndexT CFloat',
      unsafeWithPtr* `M.Array S Ix2 CFloat',
      `Int', `Int', `Float' } -> `Int' #}

{#fun flann_add_points_double as ^
    { withFlannIndexT* `FlannIndexT CDouble',
      unsafeWithPtr* `M.Array S Ix2 CDouble',
      `Int', `Int', `Float' } -> `Int' #}

{#fun flann_add_points_byte as ^
    { withFlannIndexT* `FlannIndexT CUChar',
      unsafeWithPtr* `M.Array S Ix2 CUChar',
      `Int', `Int', `Float' } -> `Int' #}

{#fun flann_add_points_int as ^
    { withFlannIndexT* `FlannIndexT CInt',
      unsafeWithPtr* `M.Array S Ix2 CInt',
      `Int', `Int', `Float' } -> `Int' #}

-- ** remove points
{#fun flann_remove_point as ^
    { withFlannIndexT* `FlannIndexT a',
      `CUInt' } -> `Int' #}

{#fun flann_remove_point_float as ^
    { withFlannIndexT* `FlannIndexT CFloat',
      `CUInt' } -> `Int' #}

{#fun flann_remove_point_double as ^
    { withFlannIndexT* `FlannIndexT CDouble',
      `CUInt' } -> `Int' #}

{#fun flann_remove_point_byte as ^
    { withFlannIndexT* `FlannIndexT CUChar',
      `CUInt' } -> `Int' #}

{#fun flann_remove_point_int as ^
    { withFlannIndexT* `FlannIndexT CInt',
      `CUInt' } -> `Int' #}

-- ** Get Point Functions
{#fun flann_get_point as ^
    { withFlannIndexT* `FlannIndexT CUInt',
      `CUInt' } -> `Ptr CFloat' id #}

{#fun flann_get_point_float as ^
    { withFlannIndexT* `FlannIndexT CUInt',
      `CUInt' } -> `Ptr CFloat' id #}

{#fun flann_get_point_double as ^
    { withFlannIndexT* `FlannIndexT CUInt',
      `CUInt' } -> `Ptr CDouble' id #}

{#fun flann_get_point_byte as ^
    { withFlannIndexT* `FlannIndexT CUInt',
      `CUInt' } -> `Ptr CUChar' id #}

{#fun flann_get_point_int as ^
    { withFlannIndexT* `FlannIndexT CUInt',
      `CUInt' } -> `Ptr CInt' id #}

-- ** Get the number of points in the index
{#fun flann_veclen as ^
    { withFlannIndexT* `FlannIndexT a' } -> `CUInt' #}

{#fun flann_veclen_float as ^
    { withFlannIndexT* `FlannIndexT CFloat' } -> `CUInt' #}

{#fun flann_veclen_double as ^
    { withFlannIndexT* `FlannIndexT CDouble' } -> `CUInt' #}

{#fun flann_veclen_byte as ^
    { withFlannIndexT* `FlannIndexT CUChar' } -> `CUInt' #}

{#fun flann_veclen_int as ^
    { withFlannIndexT* `FlannIndexT CInt' } -> `CUInt' #}

-- ** Get the memory used by the index
{#fun flann_used_memory as ^
    { withFlannIndexT* `FlannIndexT a' } -> `Int' #}

{#fun flann_used_memory_float as ^
    { withFlannIndexT* `FlannIndexT CFloat' } -> `Int' #}

{#fun flann_used_memory_double as ^
    { withFlannIndexT* `FlannIndexT CDouble' } -> `Int' #}

{#fun flann_used_memory_byte as ^
    { withFlannIndexT* `FlannIndexT CUChar' } -> `Int' #}

{#fun flann_used_memory_int as ^
    { withFlannIndexT* `FlannIndexT CInt' } -> `Int' #}

-- ** dimension (number of values per point)
{#fun flann_size as ^
  {withFlannIndexT* `FlannIndexT CFloat'} -> `CUInt' #}

{#fun flann_size_float as ^
  {withFlannIndexT* `FlannIndexT CFloat'} -> `CUInt' #}

{#fun flann_size_double as ^
  {withFlannIndexT* `FlannIndexT CDouble'} -> `CUInt' #}

{#fun flann_size_byte as ^
  {withFlannIndexT* `FlannIndexT CUChar'} -> `CUInt' #}

{#fun flann_size_int as ^
  {withFlannIndexT* `FlannIndexT CInt'} -> `CUInt' #}

-- | vector space dimension
class FlannSize a where
 size :: FlannIndexT a -> IO CUInt

instance FlannSize CFloat where size = flannSizeFloat

instance FlannSize CDouble where size = flannSizeDouble

instance FlannSize CUChar where size = flannSizeByte

instance FlannSize CInt where size = flannSizeInt

-- ** save index to file
{#fun flann_save_index_double as ^
    { withFlannIndexT* `FlannIndexT CDouble', `String' } -> `Int' #}
{#fun flann_save_index_float as ^
    { withFlannIndexT* `FlannIndexT CFloat', `String' } -> `Int' #}
{#fun flann_save_index_byte as ^
    { withFlannIndexT* `FlannIndexT CUChar', `String' } -> `Int' #}
{#fun flann_save_index_int as ^
    { withFlannIndexT* `FlannIndexT CInt', `String' } -> `Int' #}

-- ** load index from file
{- $ These should result in a 'FlannIndexT'. However it isn't clear
which 'FLANNParameters' should be used for the @env@ that is required
by 'newFlannIndex'.

Hence only a @Ptr ()@ is returned for now (which makes these rather useless).
-}
{#fun flann_load_index_double as ^
    { `String', unsafeWithPtr* `M.Array S Ix2 CDouble', `Int', `Int' } -> `Ptr ()'  id #}
{#fun flann_load_index_float as ^
    { `String', unsafeWithPtr* `M.Array S Ix2 CFloat', `Int', `Int' } -> `Ptr ()' id #}
{#fun flann_load_index_byte as ^
    { `String', unsafeWithPtr* `M.Array S Ix2 CUChar', `Int', `Int' } -> `Ptr ()' id #}
{#fun flann_load_index_int as ^
    { `String', unsafeWithPtr* `M.Array S Ix2 CInt', `Int', `Int' } -> `Ptr ()' id #}


-- ** nearest neighbour searches with index
{#fun flann_find_nearest_neighbors_index_double as ^
    { withFlannIndexT* `FlannIndexT CDouble',
      unsafeWithPtr* `M.Array S Ix2 CDouble', `Int',
      unsafeWithPtr* `M.Array S Ix2 CInt',
      unsafeWithPtr* `M.Array S Ix2 CDouble',
      `Int', withFLANNParameters* `FLANNParameters' } -> `Int' #}

{#fun flann_find_nearest_neighbors_index_float as ^
    { withFlannIndexT* `FlannIndexT CFloat',
      unsafeWithPtr* `M.Array S Ix2 CFloat', `Int',
      unsafeWithPtr* `M.Array S Ix2 CInt',
      unsafeWithPtr* `M.Array S Ix2 CFloat',
      `Int', withFLANNParameters* `FLANNParameters' } -> `Int' #}

{#fun flann_find_nearest_neighbors_index_byte as ^
    { withFlannIndexT* `FlannIndexT CUChar',
      unsafeWithPtr* `M.Array S Ix2 CUChar', `Int',
      unsafeWithPtr* `M.Array S Ix2 CInt',
      unsafeWithPtr* `M.Array S Ix2 CFloat',
      `Int', withFLANNParameters* `FLANNParameters' } -> `Int' #}

{#fun flann_find_nearest_neighbors_index_int as ^
    { withFlannIndexT* `FlannIndexT CInt',
      unsafeWithPtr* `M.Array S Ix2 CInt', `Int',
      unsafeWithPtr* `M.Array S Ix2 CInt',
      unsafeWithPtr* `M.Array S Ix2 CFloat',
      `Int', withFLANNParameters* `FLANNParameters' } -> `Int' #}


-- ** search within a radius
{#fun flann_radius_search_double as ^
    { withFlannIndexT* `FlannIndexT CDouble',
      unsafeWithPtr* `M.Array S Ix2 CDouble',
      unsafeWithPtr* `M.Array S Ix2 CInt',
      unsafeWithPtr* `M.Array S Ix2 CDouble',
      `Int',
      `Float',
      withFLANNParameters* `FLANNParameters' } -> `Int' #}

{#fun flann_radius_search_float as ^
    { withFlannIndexT* `FlannIndexT CFloat',
      unsafeWithPtr* `M.Array S Ix2 CFloat',
      unsafeWithPtr* `M.Array S Ix2 CInt',
      unsafeWithPtr* `M.Array S Ix2 CFloat',
      `Int',
      `Float',
      withFLANNParameters* `FLANNParameters' } -> `Int' #}

{#fun flann_radius_search_byte as ^
    { withFlannIndexT* `FlannIndexT CUChar',
      unsafeWithPtr* `M.Array S Ix2 CUChar',
      unsafeWithPtr* `M.Array S Ix2 CInt',
      unsafeWithPtr* `M.Array S Ix2 CFloat',
      `Int',
      `Float',
      withFLANNParameters* `FLANNParameters' } -> `Int' #}

{#fun flann_radius_search_int as ^
    { withFlannIndexT* `FlannIndexT CInt',
      unsafeWithPtr* `M.Array S Ix2 CInt',
      unsafeWithPtr* `M.Array S Ix2 CInt',
      unsafeWithPtr* `M.Array S Ix2 CFloat',
      `Int',
      `Float',
      withFLANNParameters* `FLANNParameters' } -> `Int' #}

-- ** compute cluster centers

{#fun flann_compute_cluster_centers_float as ^
    { unsafeWithPtr* `M.Array S Ix2 CFloat',
      `Int', `Int', `Int', unsafeWithPtr* `M.Array S Ix2 CFloat',
      withFLANNParameters* `FLANNParameters' } -> `Int' #}
{#fun flann_compute_cluster_centers_double as ^
    { unsafeWithPtr* `M.Array S Ix2 CDouble',
      `Int', `Int', `Int', unsafeWithPtr* `M.Array S Ix2 CDouble',
      withFLANNParameters* `FLANNParameters' } -> `Int' #}
{#fun flann_compute_cluster_centers_byte as ^
    { unsafeWithPtr* `M.Array S Ix2 CUChar',
      `Int', `Int', `Int', unsafeWithPtr* `M.Array S Ix2 CFloat',
      withFLANNParameters* `FLANNParameters' } -> `Int' #}
{#fun flann_compute_cluster_centers_int as ^
    { unsafeWithPtr* `M.Array S Ix2 CInt',
      `Int', `Int', `Int', unsafeWithPtr* `M.Array S Ix2 CFloat',
      withFLANNParameters* `FLANNParameters' } -> `Int' #}


-- ** free index
-- $ these functions with suffix 2 are the same as those without,
-- except they also free the environment pointer
foreign import ccall "&flann_free_index_double2" flann_free_index_double2 :: FunPtr (Ptr a -> Ptr b -> IO ())
foreign import ccall "&flann_free_index_float2" flann_free_index_float2 :: FunPtr (Ptr a -> Ptr b -> IO ())
foreign import ccall "&flann_free_index_byte2" flann_free_index_byte2 :: FunPtr (Ptr a -> Ptr b -> IO ())
foreign import ccall "&flann_free_index_int2" flann_free_index_int2 :: FunPtr (Ptr a -> Ptr b -> IO ())

foreign import ccall "&flann_free_index_double" flann_free_index_double :: FunPtr (Ptr a -> Ptr b -> IO ())
foreign import ccall "&flann_free_index_float" flann_free_index_float :: FunPtr (Ptr a -> Ptr b -> IO ())
foreign import ccall "&flann_free_index_byte" flann_free_index_byte :: FunPtr (Ptr a -> Ptr b -> IO ())
foreign import ccall "&flann_free_index_int" flann_free_index_int :: FunPtr (Ptr a -> Ptr b -> IO ())

-- | flann doesn't seem to do much with env besides set some logging level...
-- but regardless the whole env is copied.
newFlannIndex fn env p = do
    env' <- copyFlannParametersNoFinalizer env
    fmap FlannIndexT (newForeignPtrEnv fn env' p)

-- | passing a null pointer instead of a particular environment
newFlannIndex' fn p = do
    fmap FlannIndexT (newForeignPtrEnv fn nullPtr p)

-- ** low level manipulation of 'FLANNParameters'
withFLANNParameters (FLANNParameters x) f = withForeignPtr x f

copyFlannParametersNoFinalizer :: FLANNParameters -> IO (Ptr ())
copyFlannParametersNoFinalizer (FLANNParameters x) = do
    let n = {# sizeof FLANNParametersStruct #}
    y <- mallocBytes n
    withForeignPtr x $ \from -> copyBytes y from n
    return y

-- ** wrapper functions used to make class instances

-- | use 'findNearestNeighbors' instead
makeFindNearestNeighborsWrapper flannFn dataSet querySet nn flanps
      | Sz2 dy dx <- M.size dataSet,
        Sz2 ty tx <- M.size querySet,
        tx == dx = do
        let indices = M.replicate Seq (Sz2 nn ty) 0
        let dists = M.replicate Seq (Sz2 nn ty) 0
        retc <-
          flannFn
            dataSet
            dy dx
            querySet
            ty
            indices
            dists
            nn
            flanps
        when (retc /= 0) $ error "flann_find_nearest_neighbors returned nonzero"
        return (indices, dists)
  | otherwise = error "makeFindNearestNeighborsWrapper: nonconformable dimensions"

-- | use 'buildIndex' instead
makeBuildIndex build free arr ps |
    Sz2 ny nx <- M.size arr = do
    (idx,speedup) <- build arr ny nx ps
    idx <- newFlannIndex free ps idx
    return (idx, speedup)

-- | use 'computeClusterCenters' instead
makeComputeClusterCenters computeFn arr n ps |
   Sz2 ny nx <- M.size arr = do
    branching <- fromIntegral `fmap` get_branching ps
    let nmax = last $ takeWhile (n >=) $ Prelude.map (\k -> (branching-1)*k +1 ) [0 .. ]
        nmax2 = 1 + div (n-1) (branching-1) * (branching-1)
    when (nmax /= nmax2) $ error "nmax /= nmax2"
    let centres = M.replicate Seq (Sz2 nmax2 ny) 0

    retc <- computeFn
        arr ny nx (fromIntegral n)
        centres
        ps
    when (retc /= 0) $ error "flann_compute_cluster_centers returned nonzero"
    return (M.extract' 0 (Sz2 retc ny) centres)

-- | use 'findNearestNeighborsIndex' instead
makeFindNearestNeighborsIndex findNN idx queries nn flanps
    | Sz2 ny nx <- M.size queries = do
    let indexes = M.replicate Seq (Sz2 ny nn) 0
    let distances = M.replicate Seq (Sz2 ny nn) 0
    {- documentation says this should be just an nx array... that seems off
    because you don't whant the distances of all of the points recovered? -}

    findNN idx queries nx indexes distances nn flanps

    return (indexes, distances)

-- | use 'radiusSearch' instead
makeRadiusSearch flannFn index pt max_nn radius2 ps =  do
    dim <- fromIntegral <$> Flann.Internal.size index
    let indices = M.replicate Seq (Sz2 max_nn dim) 0
    let dists = M.replicate Seq (Sz2 max_nn dim) 0

    -- is this return code the number of neighbours??
    nneigh <- flannFn
        index
        pt
        indices
        dists
        max_nn
        radius2
        ps

    -- reallocate if nneigh << max_nn?
    return (indices, dists)

-- | use 'loadIndex' instead
makeLoadIndex freeFn loadFn f dataset
    | Sz2 x y <- M.size dataset = do
                ptr <- loadFn f dataset x y
                newFlannIndex' freeFn ptr

checkSaveIndex :: IO Int -> IO ()
checkSaveIndex action = do
  result <- action
  when (result /= 0) $ error $ "flann_save_index failed with error code: " ++ show result

-- ** could be part of c2hs
cToEnum x = toEnum (fromIntegral x)
cFromEnum x = fromIntegral (fromEnum x)

-- * Foreign function (c2hs and others) interface

-- ** enums when setting 'FLANNParameters'
{#enum flann_distance_t as ^ {} deriving (Show) #}
{#enum flann_algorithm_t as ^ {} deriving (Show) #}
{#enum flann_centers_init_t as ^ {} deriving (Show) #}
{#enum flann_log_level_t as ^ {} deriving (Show) #}


-- ** parameters
foreign import ccall "&DEFAULT_FLANN_PARAMETERS" defaultFlannParameters_ :: Ptr ()

-- ** c2hs generate Seems to be sorted by distance, so the closest indexes are in the earlier columns
