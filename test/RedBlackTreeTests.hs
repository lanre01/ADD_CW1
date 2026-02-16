{-# LANGUAGE ScopedTypeVariables #-}

module RedBlackTreeTests(runTests) where

import RedBlackTrees
import Test.HUnit
import Data.List (nub, sort)
import Control.Monad (replicateM)
import System.Random (randomRIO, newStdGen, randomRs, mkStdGen)
import Test.QuickCheck

-- Helper function to convert tree to list using Foldable instance
toList' :: RBTree a -> [a]
toList' = foldr (:) []

-- Test data
testLists :: [[Int]]
testLists = [
    [],                -- empty
    [1],               -- single element
    [1,2,3,4,5],       -- ascending order
    [5,4,3,2,1],       -- descending order
    [3,1,4,1,5,9,2,6], -- with duplicates
    [50,25,75,10,40,60,90], -- balanced tree
    [1,2,3,4,5,6,7,8,9,10], -- right-heavy
    [10,9,8,7,6,5,4,3,2,1]  -- left-heavy
  ]

-- Property-based test data
randomLists :: IO [[Int]]
randomLists = do
    g <- newStdGen
    let sizes = take 10 [1..]
    return $ map (\n -> take n $ randomRs (1, 1000) g) sizes

-- Helper to generate random operations for property testing
randomOperations :: Int -> IO [(Int, Bool)]  -- (value, isInsert)
randomOperations n = do
    vals <- replicateM n $ randomRIO (1, 100)
    ops <- replicateM n $ randomRIO (True, False)
    return $ zip vals ops

-- Test if tree maintains BST property
testBSTProperty :: Test
testBSTProperty = TestList $
    map (\xs -> TestCase $ do
        let tree = fromList xs
        assertBool ("BST property failed for: " ++ show xs) (isBST tree))
    testLists

-- Test if tree maintains RBT properties
testRBTProperty :: Test
testRBTProperty = TestList $
    map (\xs -> TestCase $ do
        let tree = fromList xs
        assertBool ("RBT property failed for: " ++ show xs) (isRBT tree))
    testLists

-- Test insert maintains invariants
testInsertInvariants :: Test
testInsertInvariants = TestCase $ do
    let testInsertions = foldl insert Nil [5,3,7,2,4,6,8,1,9]
    assertBool "Insert maintains BST" (isBST testInsertions)
    assertBool "Insert maintains RBT" (isRBT testInsertions)

-- Test delete maintains invariants
testDeleteInvariants :: Test
testDeleteInvariants = TestCase $ do
    let tree = fromList [5,3,7,2,4,6,8,1,9]
    let afterDeletion = delete tree 5
    assertBool "Delete maintains BST" (isBST afterDeletion)
    assertBool "Delete maintains RBT" (isRBT afterDeletion)

-- Test delete non-existent element
testDeleteNonExistent :: Test
testDeleteNonExistent = TestCase $ do
    let tree = fromList [1,2,3,4,5]
    let tree' = delete tree 10
    assertBool "Delete non-existent maintains BST" (isBST tree')
    assertBool "Delete non-existent maintains RBT" (isRBT tree')
    -- Test that tree is unchanged by checking all elements are still present
    let allPresent = all (\x -> contains tree' x) [1,2,3,4,5]
    let notPresent = not (contains tree' 10)
    assertBool "Tree unchanged after deleting non-existent element" 
               (allPresent && notPresent)

-- Test contains function
testContains :: Test
testContains = TestList $
    map (\xs -> TestCase $ do
        let tree = fromList xs
        let present = all (\x -> contains tree x) xs
        let notPresent = not $ any (\x -> contains tree (x + 1000)) xs
        assertBool ("Contains failed for present elements: " ++ show xs) present
        assertBool ("Contains failed for absent elements: " ++ show xs) notPresent)
    testLists

-- Test insert-delete sequence
testInsertDeleteSequence :: Test
testInsertDeleteSequence = TestCase $ do
    let tree = Nil
    let tree1 = insert tree 10
    let tree2 = insert tree1 20
    let tree3 = insert tree2 30
    let tree4 = insert tree3 40
    let tree5 = insert tree4 50
    
    assertBool "Sequence insert maintains RBT" (isRBT tree5)
    
    let tree6 = delete tree5 30
    let tree7 = delete tree6 10
    let tree8 = delete tree7 50
    
    assertBool "Sequence delete maintains RBT" (isRBT tree8)
    -- Check final elements using contains
    assertBool "Contains 20" (contains tree8 20)
    assertBool "Contains 40" (contains tree8 40)
    assertBool "Not contains 30" (not (contains tree8 30))
    assertBool "Not contains 10" (not (contains tree8 10))
    assertBool "Not contains 50" (not (contains tree8 50))

-- Test that tree remains sorted after operations (using orderedBy)
testSortedOrder :: Test
testSortedOrder = TestList $
    map (\xs -> TestCase $ do
        let tree = fromList xs
        let treeList = toList' tree
        let isSorted = orderedBy (<=) treeList
        assertBool ("Tree not sorted for: " ++ show xs) isSorted
        -- Check that all original unique elements are present
        let sortedUnique = nub $ sort xs
        let allPresent = all (\x -> contains tree x) sortedUnique
        assertBool ("Not all elements present for: " ++ show xs) allPresent)
    testLists



-- Test that insert of duplicate doesn't change tree
testInsertDuplicate :: Test
testInsertDuplicate = TestCase $ do
    let tree1 = fromList [1,2,3,4,5]
    let tree2 = insert tree1 3
    -- Check that both trees have same elements
    let checkAllElements = all (\x -> contains tree1 x && contains tree2 x) [1,2,3,4,5]
    let checkNoExtra = not (contains tree2 6)  -- element that shouldn't be there
    assertBool "Insert duplicate doesn't change elements" 
               (checkAllElements && checkNoExtra)
    assertBool "Tree remains valid RBT after duplicate insert" (isRBT tree2)

-- Run all tests
runAllTests :: IO Counts
runAllTests = runTestTT $ TestList [
    testBSTProperty,
    testRBTProperty,
    testInsertInvariants,
    testDeleteInvariants,
    testDeleteNonExistent,
    testContains,
    testInsertDeleteSequence,
    testSortedOrder,
    -- testBlackHeight,
    -- testColorInvariant,
    -- testRootBlack,
    testInsertDuplicate
  ]

-- QuickCheck style property tests
quickCheckTests :: IO ()
quickCheckTests = do
    putStrLn "Running property tests..."
    
    -- Test random sequences
    randLists <- randomLists
    let results = map (\xs -> 
            let tree = fromList xs
            in (isBST tree, isRBT tree)) randLists
    
    let allBST = all fst results
    let allRBT = all snd results
    
    putStrLn $ "All random trees are BST: " ++ show allBST
    putStrLn $ "All random trees are RBT: " ++ show allRBT
    
    -- Test random operations
    ops <- randomOperations 50
    let finalTree = foldl (\t (val, isInsert) -> 
            if isInsert then insert t val else delete t val) Nil ops
    
    putStrLn $ "Random operations maintain BST: " ++ show (isBST finalTree)
    putStrLn $ "Random operations maintain RBT: " ++ show (isRBT finalTree)
    
    -- Stress test with many elements
    let bigTree = fromList [1..1000]
    putStrLn $ "Big tree (1000 elements) is BST: " ++ show (isBST bigTree)
    putStrLn $ "Big tree (1000 elements) is RBT: " ++ show (isRBT bigTree)
    
    -- Test deletion of all elements
    let emptyTree = foldl delete bigTree [1..1000]
    putStrLn $ "After deleting all elements: " ++ 
        if toList' emptyTree == [] then "Empty ✓" else "NOT EMPTY ✗"
    
    -- Test that all elements are actually deleted
    let anyRemaining = any (\x -> contains emptyTree x) [1..1000]
    putStrLn $ "Any elements remaining after full deletion: " ++ 
        if anyRemaining then "YES ✗" else "NO ✓"

-- Additional specific edge case tests
testEdgeCases :: IO ()
testEdgeCases = do
    putStrLn "\nTesting edge cases:"
    
    -- Test single element tree
    let single = insert Nil 42
    putStrLn $ "Single element tree: BST=" ++ show (isBST single) ++ 
               ", RBT=" ++ show (isRBT single) ++
               ", Contains 42=" ++ show (contains single 42)
    
    -- Test delete from single element tree
    let empty = delete single 42
    putStrLn $ "After delete single: " ++ 
        if toList' empty == [] then "Empty ✓" else "NOT EMPTY ✗"
    
    -- Test left spine deletion
    let leftSpine = foldl insert Nil [10,9,8,7,6,5,4,3,2,1]
    let afterDeletes = foldl delete leftSpine [1..5]
    putStrLn $ "Left spine deletion: BST=" ++ show (isBST afterDeletes) ++
               ", RBT=" ++ show (isRBT afterDeletes) ++
               ", Elements=" ++ show (toList' afterDeletes)
    
    -- Test right spine deletion
    let rightSpine = foldl insert Nil [1,2,3,4,5,6,7,8,9,10]
    let afterDeletes2 = foldl delete rightSpine [10,9,8,7,6]
    putStrLn $ "Right spine deletion: BST=" ++ show (isBST afterDeletes2) ++
               ", RBT=" ++ show (isRBT afterDeletes2) ++
               ", Elements=" ++ show (toList' afterDeletes2)
    
    -- Test alternating insert/delete
    let start = Nil
    let t1 = insert start 1
    let t2 = delete t1 1
    let t3 = insert t2 2
    let t4 = delete t3 2
    let t5 = insert t4 3
    putStrLn $ "Alternating operations: BST=" ++ show (isBST t5) ++
               ", RBT=" ++ show (isRBT t5) ++
               ", Elements=" ++ show (toList' t5)
    
    -- Test fromList function
    putStrLn "\nTesting fromList function:"
    let lst = [5,2,8,1,3,7,9]
    let treeFromList = fromList lst
    putStrLn $ "fromList " ++ show lst ++ ":"
    putStrLn $ "  BST: " ++ show (isBST treeFromList)
    putStrLn $ "  RBT: " ++ show (isRBT treeFromList)
    putStrLn $ "  Contains all: " ++ show (all (\x -> contains treeFromList x) lst)

runTests :: IO ()
runTests = do
    putStrLn "Running Red-Black Tree tests..."
    putStrLn "===============================\n"
    
    counts <- runAllTests
    putStrLn $ "\nHUnit tests: " ++ show counts
    
    quickCheckTests
    testEdgeCases
    
    putStrLn "\nAll tests completed!"