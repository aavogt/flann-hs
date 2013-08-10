{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternGuards #-}
module Flann where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Control.Applicative
import Data.Vector.Storable.Mutable as VM
import Data.Vector.Storable.Mutable (unsafeWith)
import Data.Array.Repa hiding((++))
import Data.Array.Repa.Repr.ForeignPtr
import Foreign.ForeignPtr
import System.IO.Unsafe
import Foreign.Marshal.Utils
import Foreign.Ptr

import C2HS hiding (cToEnum, cFromEnum, unsafePerformIO)


#include "flann_adapter.h"



-- * High level interface


-- | flann returns distances between points specified in several different
-- types. This type function gives the type used to report the distance given
-- the type that represents the points.
--
-- This type will be 'CFloat', except if the input arrays are 'CDouble'
type family DistanceType a

type instance DistanceType CDouble = CDouble
type instance DistanceType CFloat  = CFloat
type instance DistanceType CInt    = CFloat
type instance DistanceType CUChar  = CFloat

-- | array dimensions are taken care of by use of repa
class FindNearestNeighbors t where
  findNearestNeighbors ::
        Array F DIM2 t -- ^ m-by-n matrix giving m n-dimensional points
     -> Array F DIM2 t -- ^ q-by-n matrix giving q n-dimensional points whose nearest neighbours are to be found
     -> Int            -- ^ nn number of neighbours to be found
     -> FLANNParameters
     -> IO (CInt, Array F DIM2 CInt, Array F DIM2 (DistanceType t))
            {- ^ exit code... should be an exception?,

                   q-by-nn matrix whose entries are 0-based indexes into
                   points (rows) of the dataset. Seems to be sorted by
                   distance, so the closest indexes are in the earlier
                   columns

                   distance -}

instance FindNearestNeighbors CDouble where findNearestNeighbors = makeFindNearestNeighborsWrapper flannFindNearestNeighborsDouble
instance FindNearestNeighbors CFloat  where findNearestNeighbors = makeFindNearestNeighborsWrapper flannFindNearestNeighborsFloat
instance FindNearestNeighbors CUChar  where findNearestNeighbors = makeFindNearestNeighborsWrapper flannFindNearestNeighborsByte
instance FindNearestNeighbors CInt    where findNearestNeighbors = makeFindNearestNeighborsWrapper flannFindNearestNeighborsInt

class BuildIndex t where
    buildIndex :: Array F DIM2 t -- ^ n-by-m matrix containing n m-dimensional points
                -> FLANNParameters
                ->IO (FlannIndexT t, CFloat) -- ^ ( index , speedup)

instance BuildIndex CDouble where buildIndex = makeBuildIndex flannBuildIndexDouble flann_free_index_double2 -- was 2
instance BuildIndex CFloat  where buildIndex = makeBuildIndex flannBuildIndexFloat flann_free_index_float2
instance BuildIndex CUChar  where buildIndex = makeBuildIndex flannBuildIndexByte  flann_free_index_byte2
instance BuildIndex CInt    where buildIndex = makeBuildIndex flannBuildIndexInt flann_free_index_int2


class SaveIndex t where
    saveIndex :: FlannIndexT t -> FilePath -> IO Int -- ^ exitcode

instance SaveIndex CDouble where saveIndex = flannSaveIndexDouble
instance SaveIndex CFloat where saveIndex = flannSaveIndexFloat
instance SaveIndex CUChar where saveIndex = flannSaveIndexByte
instance SaveIndex CInt where saveIndex = flannSaveIndexInt

class LoadIndex t where loadIndex :: FilePath -> Array F DIM2 t -> IO (FlannIndexT t)

instance LoadIndex CDouble where loadIndex = makeLoadIndex flann_free_index_double flannLoadIndexDouble
instance LoadIndex CFloat  where loadIndex = makeLoadIndex flann_free_index_float flannLoadIndexFloat
instance LoadIndex CUChar  where loadIndex = makeLoadIndex flann_free_index_byte flannLoadIndexByte
instance LoadIndex CInt    where loadIndex = makeLoadIndex flann_free_index_int flannLoadIndexInt


class FindNearestNeighborsIndex t where
    findNearestNeighborsIndex :: FlannIndexT t -> Array F DIM2 t -> Int -> FLANNParameters -> IO (Array F DIM2 CInt, Array F DIM2 (DistanceType t))

instance FindNearestNeighborsIndex CDouble where findNearestNeighborsIndex = makeFindNearestNeighborsIndex flannFindNearestNeighborsIndexDouble
instance FindNearestNeighborsIndex CFloat where findNearestNeighborsIndex = makeFindNearestNeighborsIndex flannFindNearestNeighborsIndexFloat
instance FindNearestNeighborsIndex CInt where findNearestNeighborsIndex = makeFindNearestNeighborsIndex flannFindNearestNeighborsIndexInt
instance FindNearestNeighborsIndex CUChar where findNearestNeighborsIndex = makeFindNearestNeighborsIndex flannFindNearestNeighborsIndexByte


class RadiusSearch t where
    radiusSearch :: FlannIndexT t
        -> Array F DIM1 t -- ^ point whose neighbours are to be found
        -> Int -- ^ maximum of nearest neighbours
        -> Float -- ^ distance or maximum radius^2 (euclidean norm)
        -> FLANNParameters
        -> IO (Array F DIM1 CInt, Array F DIM1 (DistanceType t))
            -- ^ indexes and distances of the neighbours found
instance RadiusSearch CDouble where radiusSearch = makeRadiusSearch flannRadiusSearchDouble 
instance RadiusSearch CFloat where radiusSearch = makeRadiusSearch flannRadiusSearchFloat
instance RadiusSearch CUChar where radiusSearch = makeRadiusSearch flannRadiusSearchByte
instance RadiusSearch CInt where radiusSearch = makeRadiusSearch flannRadiusSearchInt

class ComputeClusterCenters t where
    computeClusterCenters
        :: Array F DIM2 t
        -> Int -- ^ number of clusters
        -> FLANNParameters
        -> IO (Maybe (Array F DIM2 (DistanceType t)))
instance ComputeClusterCenters CDouble where computeClusterCenters = makeComputeClusterCenters flannComputeClusterCentersDouble
instance ComputeClusterCenters CFloat  where computeClusterCenters = makeComputeClusterCenters flannComputeClusterCentersFloat
instance ComputeClusterCenters CUChar  where computeClusterCenters = makeComputeClusterCenters flannComputeClusterCentersByte
instance ComputeClusterCenters CInt    where computeClusterCenters = makeComputeClusterCenters flannComputeClusterCentersInt


-- * @FLANNParameters@
{- | This should be treated as an opaque object.

It should arguably contain the 'flannSetDistanceType', which could be faked in
the haskell binding.
-}
newtype FLANNParameters = FLANNParameters { unFLANNParameters :: ForeignPtr () }





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

-- ** lenses
-- $ \'pure\' modification of a given 'FLANNParameters'. For use with "Control.Lens".

-- *** algorithm
algorithm f x = makeLens get_algorithm set_algorithm f x

-- | the last couple are abbreviations of the first
{#enum flann_algorithm_t as ^ {} deriving (Show) #}

-- *** search time

-- | how many leafs (features) to check in one search
checks f x = makeLens get_checks set_checks f x

-- | cluster boundary index. Used when searching the kmeans tree
cb_index f x = makeLens get_cb_index set_cb_index f x

-- | eps parameter for eps-knn search
eps f x = makeLens get_eps set_eps f x

-- *** kd tree

-- | number of randomized trees to use (for kdtree)
trees f x = makeLens get_trees set_trees f x
leaf_max_size f x = makeLens get_leaf_max_size set_leaf_max_size f x

-- *** kmeans

-- | branching factor (for kmeans tree)
branching f x = makeLens get_branching set_branching f x
-- | max iterations to perform in one kmeans cluetering (kmeans tree)
iterations f x = makeLens get_iterations set_iterations f x
-- | algorithm used for picking the initial cluster centers for kmeans tree
centers_init f x = makeLens get_centers_init set_centers_init f x

-- | the last couple are abbreviations of the first
{#enum flann_centers_init_t as ^ {} deriving (Show) #}

-- *** autotuned index parameters

-- | precision desired (used for autotuning, -1 otherwise)
target_precision f x = makeLens get_target_precision set_target_precision f x
-- | build tree time weighting factor
build_weight f x = makeLens get_build_weight set_build_weight f x
-- | index memory weigthing factor
memory_weight f x = makeLens get_memory_weight set_memory_weight f x
-- | what fraction of the dataset to use for autotuning
sample_fraction f x = makeLens get_sample_fraction set_sample_fraction f x

-- *** LSH parameters

-- | The number of hash tables to use
table_number_ f x = makeLens get_table_number_ set_table_number_ f x
-- | The length of the key in the hash tables
key_size_ f x = makeLens get_key_size_ set_key_size_ f x
-- | Number of levels to use in multi-probe LSH, 0 for standard LSH
multi_probe_level_ f x = makeLens get_multi_probe_level_ set_multi_probe_level_ f x

-- *** other parameters
-- | determines the verbosity of each flann function
log_level f x = makeLens get_log_level set_log_level f x

{#enum flann_log_level_t as ^ {} deriving (Show) #}

-- | random seed to use
random_seed f x = makeLens get_random_seed set_random_seed f x

-- * Probably not necessary to use (ie. private)


-- ** Find nearest neighbour
{#fun flann_find_nearest_neighbors_double as ^
    { unsafeWith* `VM.IOVector CDouble',
      `Int',
      `Int',
      unsafeWith* `VM.IOVector CDouble',
      `Int',
      unsafeWith* `VM.IOVector CInt',
      unsafeWith* `VM.IOVector CDouble',
      `Int',
      withFLANNParameters* `FLANNParameters'} -> `CInt' id #}

{#fun flann_find_nearest_neighbors_float as ^
    { unsafeWith* `VM.IOVector CFloat',
      `Int', `Int',
      unsafeWith* `VM.IOVector CFloat',
      `Int',
      unsafeWith* `VM.IOVector CInt',
      unsafeWith* `VM.IOVector CFloat',
      `Int',
      withFLANNParameters* `FLANNParameters'} -> `CInt' id #}

{#fun flann_find_nearest_neighbors_byte as ^
    { unsafeWith* `VM.IOVector CUChar',
      `Int',
      `Int',
      unsafeWith* `VM.IOVector CUChar',
      `Int',
      unsafeWith* `VM.IOVector CInt',
      unsafeWith* `VM.IOVector CFloat',
      `Int',
      withFLANNParameters* `FLANNParameters'} -> `CInt' id #}

{#fun flann_find_nearest_neighbors_int as ^
    { unsafeWith* `VM.IOVector CInt',
      `Int',
      `Int',
      unsafeWith* `VM.IOVector CInt',
      `Int',
      unsafeWith* `VM.IOVector CInt',
      unsafeWith* `VM.IOVector CFloat',
      `Int',
      withFLANNParameters* `FLANNParameters'} -> `CInt' id #}

-- ** Creating an index

{- | the type @a@ represents the type of point used to create the index

-}
newtype FlannIndexT a = FlannIndexT (ForeignPtr ())
withFlannIndexT (FlannIndexT x) = withForeignPtr x


{#fun flann_build_index_double as ^
        { unsafeWith* `VM.IOVector CDouble',
        `Int', `Int', alloca- `CFloat' peek*,
        withFLANNParameters* `FLANNParameters' } -> `(Ptr ())' id  #}


{#fun flann_build_index_float as ^
     { unsafeWith* `VM.IOVector CFloat',
       `Int', `Int', alloca- `CFloat' peek*,
       withFLANNParameters* `FLANNParameters' } -> `Ptr ()' id #}

{#fun flann_build_index_byte as ^
     { unsafeWith* `VM.IOVector CUChar',
       `Int', `Int', alloca- `CFloat' peek*,
       withFLANNParameters* `FLANNParameters' } -> `Ptr ()' id #}

{#fun flann_build_index_int as ^
     { unsafeWith* `VM.IOVector CInt',
       `Int', `Int', alloca- `CFloat' peek*,
       withFLANNParameters* `FLANNParameters' } -> `Ptr ()' id #}

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
    { `String', unsafeWith* `VM.IOVector CDouble', `Int', `Int' } -> `Ptr ()'  id #}
{#fun flann_load_index_float as ^
    { `String', unsafeWith* `VM.IOVector CFloat', `Int', `Int' } -> `Ptr ()' id #}
{#fun flann_load_index_byte as ^
    { `String', unsafeWith* `VM.IOVector CUChar', `Int', `Int' } -> `Ptr ()' id #}
{#fun flann_load_index_int as ^
    { `String', unsafeWith* `VM.IOVector CInt', `Int', `Int' } -> `Ptr ()' id #}


-- ** nearest neighbour searches with index
{#fun flann_find_nearest_neighbors_index_double as ^
    { withFlannIndexT* `FlannIndexT CDouble',
      unsafeWith* `VM.IOVector CDouble', `Int',
      unsafeWith* `VM.IOVector CInt',
      unsafeWith* `VM.IOVector CDouble',
      `Int', withFLANNParameters* `FLANNParameters' } -> `Int' #}

{#fun flann_find_nearest_neighbors_index_float as ^
    { withFlannIndexT* `FlannIndexT CFloat',
      unsafeWith* `VM.IOVector CFloat', `Int',
      unsafeWith* `VM.IOVector CInt',
      unsafeWith* `VM.IOVector CFloat',
      `Int', withFLANNParameters* `FLANNParameters' } -> `Int' #}

{#fun flann_find_nearest_neighbors_index_byte as ^
    { withFlannIndexT* `FlannIndexT CUChar',
      unsafeWith* `VM.IOVector CUChar', `Int',
      unsafeWith* `VM.IOVector CInt',
      unsafeWith* `VM.IOVector CFloat',
      `Int', withFLANNParameters* `FLANNParameters' } -> `Int' #}

{#fun flann_find_nearest_neighbors_index_int as ^
    { withFlannIndexT* `FlannIndexT CInt',
      unsafeWith* `VM.IOVector CInt', `Int',
      unsafeWith* `VM.IOVector CInt',
      unsafeWith* `VM.IOVector CFloat',
      `Int', withFLANNParameters* `FLANNParameters' } -> `Int' #}


-- ** search within a radius
{#fun flann_radius_search_double as ^
    { withFlannIndexT* `FlannIndexT CDouble',
      unsafeWith* `VM.IOVector CDouble',
      unsafeWith* `VM.IOVector CInt',
      unsafeWith* `VM.IOVector CDouble',
      `Int',
      `Float',
      withFLANNParameters* `FLANNParameters' } -> `Int' #}

{#fun flann_radius_search_float as ^
    { withFlannIndexT* `FlannIndexT CFloat',
      unsafeWith* `VM.IOVector CFloat',
      unsafeWith* `VM.IOVector CInt',
      unsafeWith* `VM.IOVector CFloat',
      `Int',
      `Float',
      withFLANNParameters* `FLANNParameters' } -> `Int' #}

{#fun flann_radius_search_byte as ^
    { withFlannIndexT* `FlannIndexT CUChar',
      unsafeWith* `VM.IOVector CUChar',
      unsafeWith* `VM.IOVector CInt',
      unsafeWith* `VM.IOVector CFloat',
      `Int',
      `Float',
      withFLANNParameters* `FLANNParameters' } -> `Int' #}

{#fun flann_radius_search_int as ^
    { withFlannIndexT* `FlannIndexT CInt',
      unsafeWith* `VM.IOVector CInt',
      unsafeWith* `VM.IOVector CInt',
      unsafeWith* `VM.IOVector CFloat',
      `Int',
      `Float',
      withFLANNParameters* `FLANNParameters' } -> `Int' #}

-- ** compute cluster centers

{#fun flann_compute_cluster_centers_float as ^
    { unsafeWith* `VM.IOVector CFloat',
      `Int', `Int', `Int', unsafeWith* `VM.IOVector CFloat',
      withFLANNParameters* `FLANNParameters' } -> `Int' #}
{#fun flann_compute_cluster_centers_double as ^
    { unsafeWith* `VM.IOVector CDouble',
      `Int', `Int', `Int', unsafeWith* `VM.IOVector CDouble',
      withFLANNParameters* `FLANNParameters' } -> `Int' #}
{#fun flann_compute_cluster_centers_byte as ^
    { unsafeWith* `VM.IOVector CUChar',
      `Int', `Int', `Int', unsafeWith* `VM.IOVector CFloat',
      withFLANNParameters* `FLANNParameters' } -> `Int' #}
{#fun flann_compute_cluster_centers_int as ^
    { unsafeWith* `VM.IOVector CInt',
      `Int', `Int', `Int', unsafeWith* `VM.IOVector CFloat',
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
      | [dy,dx] <- listOfShape (extent dataSet),
        [ty,tx] <- listOfShape (extent querySet),
        tx == dx = do -- should explain why failed?

        indices <- mallocForeignPtrArray (nn * ty)
        dists <- mallocForeignPtrArray (nn * ty) -- dists is output?
        retc <- flannFn
            (MVector 0 (toForeignPtr dataSet))
            dy dx

            (MVector 0 (toForeignPtr querySet))
            ty
            (MVector 0 indices)
            (MVector 0 dists)
            nn
            flanps
        return
            (retc,
            fromForeignPtr (ix2 nn ty) indices,
            fromForeignPtr (ix2 nn ty) dists)

-- | use 'buildIndex' instead
makeBuildIndex build free arr ps |
    [ny,nx] <- listOfShape (extent arr) = do
    (idx,speedup) <- build (MVector 0 (toForeignPtr arr)) ny nx ps
    idx <- newFlannIndex free ps idx
    return (idx, speedup)

-- | use 'computeClusterCenters' instead
makeComputeClusterCenters computeFn arr n ps |
   [ny,nx] <- listOfShape (extent arr) = do
    branching <- fromIntegral `fmap` get_branching ps
    let nmax = last $ takeWhile (n >=) $ Prelude.map (\k -> (branching-1)*k +1 ) [0 .. ]
        nmax2 = 1 + div (n-1) (branching-1) * (branching-1)
    if nmax /= nmax2 then error "nmax /= nmax2" else return ()
    centres <- mallocForeignPtrArray (fromIntegral ny * fromIntegral nmax2)

    retc <- computeFn
        (MVector 0 (toForeignPtr arr)) ny nx (fromIntegral n)
        (MVector 0 centres)
        ps
    if retc < 0  then do
                finalizeForeignPtr centres
                return Nothing
        else return $ Just (fromForeignPtr (ix2 retc ny) centres)

-- | use 'findNearestNeighborsIndex' instead
makeFindNearestNeighborsIndex findNN idx queries nn flanps
    | [ny,nx] <- listOfShape (extent queries) = do
    indexes <- mallocForeignPtrArray (ny * nn)
    distances <- mallocForeignPtrArray (ny * nn)
    {- documentation says this should be just an nx array... that seems off
    because you don't whant the distances of all of the points recovered? -}

    findNN idx (MVector 0 (toForeignPtr queries)) nx (MVector 0 indexes) (MVector 0 distances) nn flanps

    return (fromForeignPtr (ix2 nn ny) indexes,
            fromForeignPtr (ix2 nn ny) distances)

-- | use 'radiusSearch' instead
makeRadiusSearch flannFn index pt max_nn radius2 ps |
    dimension <- listOfShape (extent pt) =  do
    indices <- mallocForeignPtrArray max_nn
    dists <- mallocForeignPtrArray max_nn

    -- is this return code the number of neighbours??
    nneigh <- flannFn
        index
        (MVector 0 (toForeignPtr pt))
        (MVector 0 indices)
        (MVector 0 dists)
        max_nn
        radius2
        ps

    -- reallocate if nneigh << max_nn?
    return $ (fromForeignPtr (ix1 nneigh) indices,
              fromForeignPtr (ix1 nneigh) dists)

-- | use 'loadIndex' instead
makeLoadIndex freeFn loadFn f dataset
    | [x,y] <- listOfShape (extent dataset) = do
                ptr <- loadFn f (MVector 0 (toForeignPtr dataset)) x y
                newFlannIndex' freeFn ptr

-- ** could be part of c2hs
cToEnum x = toEnum (fromIntegral x)
cFromEnum x = fromIntegral (fromEnum x)

-- * Foreign function (c2hs and others) interface

{#fun flann_set_distance_type as ^ { cFromEnum `FlannDistanceT', `Int' } -> `()' #}

{#enum flann_distance_t as ^ {} deriving (Show) #}


-- ** parameters
foreign import ccall "&DEFAULT_FLANN_PARAMETERS" defaultFlannParameters_ :: Ptr ()

-- | these unsafePerformIOs are safe?
makeLens get set f x = unsafePerformIO $ do
    field <- get x
    y <- copyFlannParameters x
    return $ fmap (\field' -> unsafePerformIO $ do
        set field' y
        return y) (f field)

-- *** unsafe IO getters
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

-- *** unsafe IO setters
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


-- ** c2hs generated
