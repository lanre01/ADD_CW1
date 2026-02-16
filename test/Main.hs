{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.QuickCheck
import Data.List (sort, nub)

import RedBlackTrees
  ( Colour(..)
  , RBTree(..)
  , fromList
  , isBST
  , isRBT
  , insert
  , delete
  , contains
  )

-- ----------------------------
-- Helpers: inorder + list model
-- ----------------------------

-- Inorder traversal using only exported constructors
inorder :: RBTree a -> [a]
inorder Nil = []
inorder (Node x _ l r) = inorder l ++ [x] ++ inorder r

-- Your tree behaves like a SET (duplicates ignored).
modelFromList :: Ord a => [a] -> [a]
modelFromList = sort . nub

modelInsert :: Ord a => a -> [a] -> [a]
modelInsert x xs = modelFromList (x : xs)

modelDelete :: Ord a => a -> [a] -> [a]
modelDelete x xs = filter (/= x) xs  -- already sorted+unique in our usage

-- --------------------------------
-- Properties (core invariants/spec)
-- --------------------------------

-- Building from a list should produce a valid BST and RBT
prop_fromList_valid :: [Int] -> Property
prop_fromList_valid xs =
  let t = fromList xs
  in counterexample ("tree=" ++ show t) $
       isBST t .&&. isRBT t

-- Inorder traversal should match the set model (sorted, unique)
prop_fromList_inorder_matches_model :: [Int] -> Property
prop_fromList_inorder_matches_model xs =
  let t = fromList xs
  in counterexample ("inorder=" ++ show (inorder t) ++
                     "\nmodel=" ++ show (modelFromList xs)) $
       inorder t === modelFromList xs

-- contains should agree with model membership
prop_contains_matches_model :: [Int] -> Int -> Property
prop_contains_matches_model xs x =
  let t     = fromList xs
      model = modelFromList xs
  in counterexample ("tree=" ++ show t) $
       contains t x === (x `elem` model)

-- Inserting one element preserves invariants and matches model
prop_insert_preserves_invariants_and_semantics :: [Int] -> Int -> Property
prop_insert_preserves_invariants_and_semantics xs x =
  let t  = fromList xs
      t' = insert t x
      model' = modelInsert x (modelFromList xs)
  in conjoin
      [ counterexample "BST failed after insert" (property (isBST t'))
      , counterexample "RBT failed after insert" (property (isRBT t'))
      , counterexample "inorder != model after insert" (inorder t' === model')
      , counterexample "contains mismatch after insert"
          (contains t' x === True)
      ]

-- Deleting one element preserves invariants and matches model
prop_delete_preserves_invariants_and_semantics :: [Int] -> Int -> Property
prop_delete_preserves_invariants_and_semantics xs x =
  let t  = fromList xs
      t' = delete t x
      model0 = modelFromList xs
      model' = modelDelete x model0
  in conjoin
      [ counterexample "BST failed after delete" (property (isBST t'))
      , counterexample "RBT failed after delete" (property (isRBT t'))
      , counterexample "inorder != model after delete" (inorder t' === model')
      , counterexample "contains should be false after delete"
          (contains t' x === False)
      ]

-- -------------------------------
-- Sequence testing (strongest one)
-- -------------------------------

data Op = Ins Int | Del Int deriving (Show)

instance Arbitrary Op where
  arbitrary = oneof [Ins <$> arbitrary, Del <$> arbitrary]
  shrink (Ins x) = Ins <$> shrink x
  shrink (Del x) = Del <$> shrink x

applyOpTree :: RBTree Int -> Op -> RBTree Int
applyOpTree t (Ins x) = insert t x
applyOpTree t (Del x) = delete t x

applyOpModel :: [Int] -> Op -> [Int]
applyOpModel xs (Ins x) = modelInsert x xs
applyOpModel xs (Del x) = modelDelete x xs

prop_ops_sequence_matches_model_and_invariants :: [Op] -> Property
prop_ops_sequence_matches_model_and_invariants ops =
  let tFinal = foldl applyOpTree (fromList []) ops
      mFinal = foldl applyOpModel [] ops
  in conjoin
      [ counterexample ("Final tree=" ++ show tFinal) (property (isBST tFinal))
      , counterexample ("Final tree=" ++ show tFinal) (property (isRBT tFinal))
      , counterexample ("Final inorder=" ++ show (inorder tFinal) ++
                        "\nFinal model=" ++ show mFinal)
          (inorder tFinal === mFinal)
      , counterexample "contains mismatch on a sampled key" $
          forAll arbitrary $ \(x :: Int) ->
            contains tFinal x === (x `elem` mFinal)
      ]

-- -------------
-- Test runner
-- -------------

main :: IO ()
main = do
  quickCheck prop_fromList_valid
  quickCheck prop_fromList_inorder_matches_model
  quickCheck prop_contains_matches_model
  quickCheck prop_insert_preserves_invariants_and_semantics
  quickCheck prop_delete_preserves_invariants_and_semantics
  quickCheck prop_ops_sequence_matches_model_and_invariants
