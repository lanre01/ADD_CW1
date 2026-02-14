module BST (
    BST (..),
    insert,
    delete,
    contains,
    toList,
    fromList,
    isBST
) where 


data BST a = Nil | Node a (BST a) (BST a) deriving(Show)


instance Foldable BST where
  foldMap _ Nil = mempty
  foldMap f (Node x l r) = foldMap f l <> f x <> foldMap f r


-- |Constructs a BST tree from a list of values.
fromList :: (Ord a) => [a] -> BST a 
fromList xs = let t = Nil in foldl insert Nil xs

-- Constructs a list from a BST
toList :: Ord a => BST a -> [a]
toList = foldr (:) []



-- |Returns true if the argument is a valid binary search tree.
isBST :: (Ord a) => BST a -> Bool
isBST = (orderedBy (<=)) . toList

value :: BST a -> Maybe a  
value Nil = Nothing 
value (Node a _ _) = Just a  



orderedBy :: (a -> a -> Bool) -> [a] -> Bool
orderedBy _ []       = True
orderedBy _ [_]      = True
orderedBy f (x:y:xs) = f x y && orderedBy f (y:xs)


-- Inserts a key into the BST
insert :: Ord a => BST a -> a -> BST a 
insert Nil key = Node key Nil Nil 
insert t@(Node a lt rt) key | key < a = Node a (insert lt key) rt 
                            | key > a = Node a lt (insert rt key)
                            | otherwise = t

-- Returns true if a key exists in the BST else false
contains :: Ord a => BST a -> a -> Bool     
contains Nil _ = False 
contains (Node a lt rt) key | key < a = contains lt key 
                            | key > a = contains rt key 
                            | otherwise = True 

-- Deletes a key from the BST if presents
delete :: Ord a => BST a -> a -> BST a 
delete Nil _ = Nil 
delete t@(Node a lt rt) key | key < a = Node a (delete lt key) rt 
                            | key > a = Node a lt (delete rt key)
                            | otherwise = del t   

del :: Ord a => BST a -> BST a 
del (Node _ Nil Nil) = Nil 
del (Node _ lt Nil)  = lt 
del (Node _ Nil rt)  = rt  
del (Node _ lt rt)   = Node a lt rt' 
               where (a, rt') = successor rt 

successor :: Ord a => BST a -> (a, BST a)
successor (Node a Nil rt) = (a, rt)
successor (Node a lt rt)  = (b, Node a lt' rt)
                    where (b, lt') = successor lt  
