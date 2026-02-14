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

sizes :: [Int]
sizes = map (2^) [10..20] 

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
      let 
        !bst = BST.fromList xs
        !rbt = RBT.fromList xs
        keys = take 1000 xs
      pure (xs, bst, rbt, keys)

main :: IO ()
main = defaultMain
  [ bgroup "insert"
      [ env (randomListIO n) $ \xs ->
          bgroup (show n)
            [ bench "BST" $ nf BST.fromList xs
            , bench "RBT" $ nf RBT.fromList xs
            ]
      | n <- sizes
      ]

  , bgroup "contains-1000"
      [ env (mkEnv n) $ \ ~( _xs, bst, rbt, keys ) ->
          bgroup (show n)
            [ bench "BST" $
                nf (\ks -> foldl' (\acc k -> BST.contains bst k `seq` acc) () ks) keys
            , bench "RBT" $
                nf (\ks -> foldl' (\acc k -> RBT.contains rbt k `seq` acc) () ks) keys
            ]
      | n <- sizes
      ]

  , bgroup "delete-1000"
      [ env (mkEnv n) $ \ ~( _xs, bst, rbt, keys ) ->
          bgroup (show n)
            [ bench "BST" $ nf (\ks -> foldl' BST.delete bst ks) keys
            , bench "RBT" $ nf (\ks -> foldl' RBT.delete rbt ks) keys
            ]
      | n <- sizes
      ]
  ]


-- cabal bench --benchmark-options="--csv=bench-output/results.csv"
-- cabal bench --benchmark-options="--csv=BST.csv --match=BST"
-- cabal bench --benchmark-options="--csv=RBT.csv --match=RBT"
