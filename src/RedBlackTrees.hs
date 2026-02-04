module RedBlackTrees (
    Colour (..),
    RBTree (..),
    fromList,
    isBST,
    isRBT,
    insert,
    delete,
    contains
) where

-- |The colour of a red-black tree
data Colour = Red | Black deriving (Show, Eq)

-- |A red-black tree
data RBTree a 
    = Nil -- ^An 'empty' node
    | Node a Colour (RBTree a) (RBTree a) deriving (Show) -- ^A node with a value, a colour, and two children

-- |Constructs a red-black tree from a list of values.
fromList :: (Ord a) => [a] -> RBTree a 
fromList xs = let t = Nil in foldl insert Nil xs

-- |Returns true if the argument is a valid binary search tree.
isBST :: (Ord a) => RBTree a -> Bool
isBST Nil = True 
isBST (Node a _ lt rt) =  if valueInvariant a lv rv 
                          then isBST lt && isBST rt  
                          else False
                          where lv = value lt; rv = value rt 

value :: RBTree a -> Maybe a 
value Nil = Nothing 
value (Node a _ _ _) = Just a 

valueInvariant  :: Ord a => a -> Maybe a -> Maybe a -> Bool 
valueInvariant a lv rv = case lv of 
                        Nothing -> case rv of
                                    Nothing  -> True 
                                    Just r  -> a <= r
                        Just l -> case rv of 
                                    Nothing  -> a >= l 
                                    Just r  -> a >= l && a <= r 

colourInvariant :: Colour -> Colour -> Colour -> Bool 
colourInvariant Black _ _ =  True 
colourInvariant Red lc rc   = not (isRed lc) && not (isRed rc) 


-- |Returns true if the argument is a valid red-black tree.
isRBT :: (Ord a) => RBTree a -> Bool 
isRBT Nil = True  -- need to be a bst and then black height preserved
isRBT (Node a c lt rt) = if colourInvariant c lc rc && valueInvariant a lv rv 
                         then isRBT lt && isRBT rt 
                         else False
                         where lc = colour lt; rc = colour rt 
                               lv = value lt; rv = value rt 

isRed :: Colour -> Bool 
isRed c = c == Red

isBlack :: Colour -> Bool 
isBlack c = c == Black


colour :: RBTree a -> Colour
colour Nil = Black 
colour (Node _ c _ _) = c 



-- |Inserts a new element into the correct place in the tree.
insert :: (Ord a) => RBTree a -> a -> RBTree a 
insert _ _ = error "Not implemented"

-- |Deletes an element from the tree, if it is present.
delete :: (Ord a) => RBTree a -> a -> RBTree a 
delete _ _ = error "Not implemented"

-- |Returns true if the tree contains the given element.
contains :: (Ord a) => RBTree a -> a -> Bool 
contains Nil _ =  False
contains (Node a _ lt rt) key | a > key   = contains lt key 
                              | a < key   = contains rt key 
                              | otherwise = True  