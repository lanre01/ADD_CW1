module Main where

import qualified MyLib (someFunc)
import RedBlackTrees
import BST
import Criterion.Main


main :: IO ()
main = do
  let t = fromList [1..10]
  let t1 = delete t 10
  print t1 
  MyLib.someFunc
