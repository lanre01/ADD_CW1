{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.QuickCheck
import Data.List (sort, nub, foldl')
import RedBlackTrees

-- Inorder traversal 
inorder :: RBTree a -> [a]
inorder t = go t []
  where
    go Nil acc = acc
    go (Node x _ l r) acc = go l (x : go r acc)

-- returns new list without duplicates
modelFromList :: Ord a => [a] -> [a]
modelFromList = sort . nub

-- inserts an element into the list ignoring duplicates just like RBTrees
modelInsert :: Ord a => a -> [a] -> [a]
modelInsert x xs = modelFromList (x : xs)

-- removes an element from the list if present
modelDelete :: Ord a => a -> [a] -> [a]
modelDelete x xs = filter (/= x) xs 


prop_fromList_must_produce_valid_RBT :: [Int] -> Property
prop_fromList_must_produce_valid_RBT xs =
  let t = fromList xs
  in 
    counterexample ("tree=" ++ show t) (property (isRBT t))

prop_value_invariant_must_be_preserved :: [Int] -> Property
prop_value_invariant_must_be_preserved xs =
  let t = fromList xs
  in counterexample ("inorder=" ++ show (inorder t) ++
                     "\nmodel=" ++ show (modelFromList xs)) $
       inorder t === modelFromList xs

prop_contains_must_return_true_for_valid_element :: [Int] -> Int -> Property
prop_contains_must_return_true_for_valid_element xs x =
  let t     = fromList xs
      model = modelFromList xs
  in counterexample ("tree=" ++ show t) $
       contains t x === (x `elem` model)

prop_insert_must_preserve_RBT_invariant :: [Int] -> Int -> Property
prop_insert_must_preserve_RBT_invariant xs x =
  let t  = fromList xs
      t' = insert t x
      model' = modelInsert x (modelFromList xs)
  in conjoin
      [ counterexample "RBT failed after insert" (property (isRBT t'))
      , counterexample "inorder != model after insert" (inorder t' === model')
      , counterexample "contains mismatch after insert"
          (contains t' x === True)
      ]


prop_delete_must_preserves_RBT_invariant :: [Int] -> Int -> Property
prop_delete_must_preserves_RBT_invariant xs x =
  let t  = fromList xs
      t' = delete t x
      model0 = modelFromList xs
      model' = modelDelete x model0
  in conjoin
      [ counterexample "RBT failed after delete" (property (isRBT t'))
      , counterexample "inorder != model after delete" (inorder t' === model')
      , counterexample "contains should be false after delete"
          (contains t' x === False)
      ]

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
  let tFinal = foldl' applyOpTree Nil ops
      mFinal = foldl' applyOpModel [] ops
  in conjoin
      [ counterexample ("Final tree=" ++ show tFinal) (property (isRBT tFinal))
      , counterexample ("Final inorder=" ++ show (inorder tFinal) ++
                        "\nFinal model=" ++ show mFinal)
          (inorder tFinal === mFinal)
      , counterexample "contains mismatch on a sampled key" $
          forAll arbitrary $ \(x :: Int) ->
            contains tFinal x === (x `elem` mFinal)
      ]

main :: IO ()
main = do
  quickCheck prop_fromList_must_produce_valid_RBT
  quickCheck prop_value_invariant_must_be_preserved
  quickCheck prop_contains_must_return_true_for_valid_element
  quickCheck prop_insert_must_preserve_RBT_invariant
  quickCheck prop_delete_must_preserves_RBT_invariant
  quickCheck prop_ops_sequence_matches_model_and_invariants
