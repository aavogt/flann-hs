{- | this generates a graph if you have R (with the ggplot package) installed
which shows roughly that the binding doesn't do a good job (or that the wrong
settings have been used or something else is wrong somewhere along the way:

         ¯\_(ツ)_/¯

-}
import Data.Csv
import Flann
import Data.Csv
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Vector as V

import System.Process


import Data.Array.Repa hiding ((++), map)
import Data.Array.Repa.Repr.Vector
import Foreign.C
import Data.Foldable (for_)
import Data.Traversable (for)
import Control.Lens

main = do
    let n = 1000
        d = 2
        nn = 2
    readProcess "/usr/bin/R" ["--no-save"] $ "write.csv(file='points.csv', matrix(runif("++ show (d*n) ++"), ncol="++ show d++"), row.names=F)"
    Right pts <- decode True `fmap` L.readFile "points.csv"
    return () `const` print (pts :: V.Vector (V.Vector Double))
    let f = copyS $ (fromVector (ix2 d n) $ V.concat ( Prelude.map (V.map CDouble)  $ V.toList pts))

    (ec, a, b) <- do
        p <- newFlannParameters
        findNearestNeighbors f f nn $ set target_precision 0.9999 p

    print (ec)
    print (extent f)
    print (extent a)

    let ds =  L8.unlines
            [ L8.unwords $ map (L8.pack . show) [x,y]
                    | i <- [0 .. n-1],
                      let x:xs = toList $ slice a (Any :. All :. i),
                      y <- xs ]
    L8.writeFile "points_conn.txt" $ L8.append (L8.pack "i j\n") ds


    let fi x = fromIntegral x
        ls = L8.unlines
            [ L8.unwords $ map (L8.pack . show) $ ([theta, dist, fi k, fi x]++) $ V.toList $ pts V.! x
                    | i <- [0 .. n-1],
                      let x:xs = toList $ slice a (Any :. All :. i),
                      (j,k) <- xs `zip` [ 0 .. ],
                      let (dist,theta) =
                            let a = pts V.! i
                                b = pts V.! fi j
                                n x y = V.sum $ V.zipWith (*) x y
                                s x y = V.zipWith (-) x y
                            in (sqrt $ V.sum $ V.zipWith (\a b -> (a-b)^2 ) a b,
                                acos (n a (s b a) / sqrt(n a a * n (s b a) (s b a))) / pi),
                      x <- [i,fi j]]
    L8.writeFile "points_conns.txt" $ L8.append (L8.pack $ "theta d k ij " ++ unwords [ "V"++show i | i <- [1 .. d]]
                                                            ++ "\n")
                                            ls
    print ec
    readProcess "R" ["--no-save"] "library(ggplot2)\n\
        \png(width=1920,height=1080)\n\
        \plot(ggplot( read.table('points_conns.txt', header=T),\
                     \aes(V1, V2, alpha=d, group=k, col=theta))\
                     \+ geom_path()\
                     \+ scale_colour_gradientn(colours=rainbow(4)))\n\
        \dev.off()"
