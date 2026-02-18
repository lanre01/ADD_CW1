{-# LANGUAGE BangPatterns #-}

import Criterion.Main
import Control.DeepSeq (NFData(..))
import Data.List (foldl')
import System.Random (mkStdGen, randoms, randomIO)

import qualified BST
import qualified RedBlackTrees as RBT

-- Deterministic list from a seed
randomListIO :: Int -> IO [Int]
randomListIO n = sequence (replicate n randomIO)

randomList :: Int -> [Int]
randomList n = take n (randoms (mkStdGen 42) :: [Int])

sizes :: [Int]
sizes = map (2^) [10..20] 

sizesR :: [Int]
sizesR = map (2^) [10..30] 

-- NFData instances so Criterion can force structures when needed
instance NFData a => NFData (BST.BST a) where
  rnf BST.Nil = ()
  rnf (BST.Node x l r) = rnf x `seq` rnf l `seq` rnf r

instance NFData a => NFData (RBT.RBTree a) where
  rnf RBT.Nil = ()
  rnf (RBT.Node x c l r) = rnf x `seq` rnf c `seq` rnf l `seq` rnf r

instance NFData RBT.Colour where
  rnf RBT.Red   = ()
  rnf RBT.Black = ()

-- Build both trees from same xs
mkEnv :: Int -> IO ([Int], BST.BST Int, RBT.RBTree Int, [Int])
mkEnv n = do
      xs  <- randomListIO n
      ys  <- randomListIO (n `div` 4)
      let 
        !bst = BST.fromList xs
        !rbt = RBT.fromList xs
        keys = ys ++ take (n `div` 4) xs  
      pure (xs, bst, rbt, keys)

sortedList :: Int -> [Int]
sortedList  n = [1..n]

-- Build both trees from same xs
mkEnv2 :: Int -> IO ([Int], BST.BST Int, RBT.RBTree Int, [Int])
mkEnv2 n =
      let 
        xs   = sortedList n
        !bst = BST.fromList 
        !rbt = RBT.fromList xs   
        keys = randomList (n `div` 4) ++ take (n `div` 4) xs  
        in 
      return (xs, bst, rbt, keys)


main :: IO ()
main = defaultMain
  [ bgroup "insert-random"
      [ env (randomListIO n) $ \xs ->
          bgroup (show n)
            [ bench "BST" $ nf BST.fromList xs
            , bench "RBT" $ nf RBT.fromList xs
            ]
      | n <- sizesR
      ]
  , bgroup "insert-sorted"
    [
      env (pure (sortedList n)) $ \xs -> 
        bgroup (show n)
            [ bench "BST" $ nf BST.fromList xs
            , bench "RBT" $ nf RBT.fromList xs
            ]
      | n <- sizes, n > 32768
    ]
  , bgroup "contains-random"
      [ env (mkEnv n) $ \ ~( _xs, bst, rbt, keys ) ->
          bgroup (show n)
            [ bench "BST" $
                nf (\ks -> foldl' (\acc k -> BST.contains bst k `seq` acc) () ks) keys
            , bench "RBT" $
                nf (\ks -> foldl' (\acc k -> RBT.contains rbt k `seq` acc) () ks) keys
            ]
      | n <- sizesR
      ]
  , bgroup "contains-sorted"
      [ env (mkEnv2 n) $ \ ~( _xs, bst, rbt, keys ) ->
          bgroup (show n)
            [ bench "BST" $
                nf (\ks -> foldl' (\acc k -> BST.contains bst k `seq` acc) () ks) keys
            , bench "RBT" $
                nf (\ks -> foldl' (\acc k -> RBT.contains rbt k `seq` acc) () ks) keys
            ]
      | n <- sizes
      ]

  , bgroup "delete-random"
      [ env (mkEnv n) $ \ ~( _xs, bst, rbt, keys ) ->
          bgroup (show n)
            [ bench "BST" $ nf (\ks -> foldl' BST.delete bst ks) keys
            , bench "RBT" $ nf (\ks -> foldl' RBT.delete rbt ks) keys
            ]
      | n <- sizesR
      ]
  , bgroup "delete-sorted"
      [ env (mkEnv2 n) $ \ ~( _xs, bst, rbt, keys ) ->
          bgroup (show n)
            [ bench "BST" $ nf (\ks -> foldl' BST.delete bst ks) keys
            , bench "RBT" $ nf (\ks -> foldl' RBT.delete rbt ks) keys
            ]
      | n <- sizes
      ]
  ]


-- cabal bench --benchmark-options="--csv=bench-output/results.csv"
-- cabal bench --benchmark-options="+RTS -s -RTS --csv=bench-output/RBT.csv --match pattern RBT"
-- cabal bench --benchmark-options="--csv=bench-output/BST.csv --match pattern BST"
-- --benchmark-options="+RTS -s -RTS"
-- cabal bench --benchmark-options="+RTS -s -RTS --csv=bench-output/insert-sorted.csv --match pattern insert-sorted"
-- cabal bench --benchmark-options="+RTS -s -RTS --csv=bench-output/delete-sorted.csv --match pattern delete-sorted"
-- cabal bench --benchmark-options="+RTS -s -RTS --csv=bench-output/delete-random.csv --match pattern delete-random"
-- cabal bench --benchmark-options="+RTS -s -RTS --csv=bench-output/contains-random.csv --match pattern contains-random"
-- cabal bench --benchmark-options="+RTS -s -RTS --csv=bench-output/contains-sorted.csv --match pattern contains-sorted"

