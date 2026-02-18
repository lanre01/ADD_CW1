module Main where

import qualified MyLib (someFunc)
import qualified RedBlackTrees
import qualified BST



main :: IO ()
main = do
  let t = RedBlackTrees.fromList [1..10]
  let t1 = RedBlackTrees.delete t 10
  print t1 
  MyLib.someFunc
