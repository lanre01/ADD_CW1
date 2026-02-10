module RedBlackTrees (
    Colour (..),
    RBTree (..),
    fromList,
    isBST,
    isRBT,
    insert,
    delete,
    contains,
    -- added for testing
    successor,
    del,
    orderedBy
) where



{-
Notes
pattern synonym extension

-}
-- |The colour of a red-black tree
data Colour = Red | Black deriving (Show, Eq)

-- |A red-black tree
data RBTree a 
    = Nil -- ^An 'empty' node
    | Node a Colour (RBTree a) (RBTree a) deriving (Show) -- ^A node with a value, a colour, and two children

instance Foldable RBTree where
  foldMap _ Nil = mempty
  foldMap f (Node x _ l r) = foldMap f l <> f x <> foldMap f r


-- |Constructs a red-black tree from a list of values.
fromList :: (Ord a) => [a] -> RBTree a 
fromList xs = let t = Nil in foldl insert Nil xs

toList :: Ord a => RBTree a -> [a]
toList = foldr (:) []

-- |Returns true if the argument is a valid binary search tree.
isBST :: (Ord a) => RBTree a -> Bool
isBST = (orderedBy (<=)) . toList

value :: RBTree a -> Maybe a 
value Nil = Nothing 
value (Node a _ _ _) = Just a 



orderedBy :: (a -> a -> Bool) -> [a] -> Bool
orderedBy _ []       = True
orderedBy _ [_]      = True
orderedBy f (x:y:xs) = f x y && orderedBy f (y:xs)

bhInvariant :: RBTree a -> Bool
bhInvariant t = go t /= Nothing
  where
    go Nil = Just 1
    go (Node _ c lt rt) = do
      hl <- go lt
      hr <- go rt
      if hl == hr
      then Just (hl + if c == Black then 1 else 0)
      else Nothing



colourInvariant :: RBTree a -> Bool 
colourInvariant Nil =  True 
colourInvariant (Node _ Red lt rt) = if not (isRedNode lt) && not (isRedNode rt) 
                                     then colourInvariant lt && colourInvariant rt 
                                     else False 
colourInvariant (Node _ _ lt rt) = colourInvariant lt && colourInvariant rt 

-- |Returns true if the argument is a valid red-black tree.
isRBT :: (Ord a) => RBTree a -> Bool 
isRBT t = isBlackNode t && allChecks t Nothing Nothing /= Nothing

allChecks :: Ord a => RBTree a -> Maybe a -> Maybe a -> Maybe Int
allChecks Nil _ _ = Just 1  
allChecks (Node x c lt rt) lo hi = do
                            withinBound lo hi x 
                            redInvariant c lt rt 
                            hl <- allChecks lt lo (Just x)
                            hr <- allChecks rt (Just x) hi

                            if hl == hr then pure () else Nothing

                            pure (hl + if isBlack c then 1 else 0)

withinBound :: Ord a => Maybe a -> Maybe a -> a -> Maybe ()
withinBound Nothing  Nothing  _ = Just ()
withinBound (Just lo) Nothing v = if lo < v then pure () else Nothing
withinBound Nothing  (Just hi) v = if v < hi then pure () else Nothing
withinBound (Just lo) (Just hi) v = if lo < v && v < hi then pure () else Nothing

redInvariant :: Colour -> RBTree a -> RBTree a -> Maybe ()
redInvariant Black _ _   = pure ()
redInvariant Red   lt rt = if isRedNode lt || isRedNode rt then Nothing 
                            else pure () 

isRed :: Colour -> Bool 
isRed c = c == Red

isRedNode :: RBTree a -> Bool 
isRedNode = isRed . colour 

isBlack :: Colour -> Bool 
isBlack c = c == Black

isBlackNode :: RBTree a -> Bool
isBlackNode = isBlack . colour  

colour :: RBTree a -> Colour
colour Nil = Black 
colour (Node _ c _ _) = c 

-- Contructuor
node :: Ord a => a -> Colour -> RBTree a -> RBTree a -> RBTree a 
node a c x y = Node a c x y



-- |Inserts a new element into the correct place in the tree.
insert :: (Ord a) => RBTree a -> a -> RBTree a 
insert tree a = blacken (insert' tree a) 
        where
        insert' Nil x = Node x Red Nil Nil
        insert' t@(Node b c lt rt) x | x < b = balanceL b c (insert' lt x) rt 
                                     | x > b = balanceR b c lt (insert' rt x)
                                     | otherwise = t -- ignore duplicate values   

balanceL :: Ord a => a -> Colour -> RBTree a -> RBTree a -> RBTree a 
balanceL a Black (Node b Red (Node c Red lllt llrt) lrt) rt = Node b Red (Node c Black lllt llrt) (Node a Black lrt rt)
balanceL a Black (Node b Red llt (Node c Red lrlt lrrt)) rt = Node c Red (Node b Black llt lrlt) (Node a Black lrrt rt)
balanceL a c lt rt = Node a c lt rt 

balanceR :: Ord a => a -> Colour -> RBTree a -> RBTree a -> RBTree a 
balanceR a Black lt (Node b Red rlt (Node c Red rrlt rrrt)) = Node b Red (Node a Black lt rlt) (Node c Black rrlt rrrt)
balanceR a Black lt (Node b Red (Node c Red rrlt rrrt) rrt) = Node c Red (Node a Black lt rrlt) (Node b Black rrrt rrt) 
balanceR a c lt rt = Node a c lt rt 

{-
Red node deletion
case at in the middle with left and right nodes
 - replace by successor 
 - do some rotation
-}

blacken :: RBTree a -> RBTree a 
blacken (Node a Red lt rt) = Node a Black lt rt 
blacken tree               = tree 

redden :: RBTree a -> RBTree a 
redden (Node a Black lt rt) = Node a Red lt rt 
redden tree                 = tree 


-- |Deletes an element from the tree, if it is present.
delete :: (Ord a) => RBTree a -> a -> RBTree a 
delete t key = (blacken . fst . delete') t 
         where 
            delete' Nil                  = (Nil, False)
            delete' t@(Node a col lt rt) | key < a  = balanceHeightR lb (Node a col lt' rt)
                                         | key > a  = balanceHeightL rb (Node a col lt rt')
                                         | otherwise = del t 
                                         where 
                                               (lt', lb) = delete' lt 
                                               (rt', rb) = delete' rt 
                                            
balanceHeightR :: (Ord a) => Bool -> RBTree a -> (RBTree a, Bool)
balanceHeightR _     Nil = error "cannot balance an empty node"
balanceHeightR False t   = (balance t, False)
balanceHeightR b (Node a col lt (Node y Black rlt rrt)) = (balance (Node a col lt (Node y Red rlt rrt)), b')
                                            where b' = if isBlack col then True else False
balanceHeightR b (Node a col lt (Node y Red rlt rrt)) = let (lt', _) = balanceHeightR b (Node a Red lt rlt) in 
                                                           (balance (Node y Black lt' rrt), b)

balanceHeightL :: (Ord a) => Bool -> RBTree a -> (RBTree a, Bool)
balanceHeightL _     Nil = error "cannot balance an empty node"
balanceHeightL False t   = (balance t, False)
balanceHeightL b     (Node a col (Node y Black llt lrt) rt) = (balance (Node a col (Node y Red llt lrt) rt), b')
                                                where b' = if isBlack col then True else False
balanceHeightL b     (Node a col (Node y Red llt lrt) rt)   = let (rt', _) = balanceHeightL b (Node a Red lrt rt) in 
                                                                (balance (Node y Black llt rt'), b)                                       


del :: (Ord a) => RBTree a -> (RBTree a, Bool)
del (Node _ Red Nil Nil)    = (Nil, False) 
del (Node _ Black Nil Nil) = (Nil, True) -- need to update the parents of black height invariant
del (Node _ Black lt Nil)  = (blacken lt, False) 
del (Node _ Black Nil rt)  = (blacken rt, False)
del (Node _ c lt rt)       = (nd, b')  -- bubbleR (Node x c lt rt')
                 where (x, b, rt') = successor rt 
                       (nd, b')     = balanceHeightL b (Node x c lt rt')

successor :: (Ord a) => RBTree a -> (a, Bool, RBTree a )
successor Nil                 = error "No successor to remove"
successor t@(Node a _ Nil _)  = (a, short, t') 
                    where (t', short)    = del t 
successor (Node a c lt rt)    =  (x, b', nd)
                where 
                      (x, b, lt') = successor lt    
                      (nd, b')    = balanceHeightR b (Node a c lt' rt)




-- |Returns true if the tree contains the given element.
contains :: (Ord a) => RBTree a -> a -> Bool 
contains Nil _ =  False
contains (Node a _ lt rt) key | a > key   = contains lt key 
                              | a < key   = contains rt key 
                              | otherwise = True  



balance :: RBTree a -> RBTree a 
balance (Node a col (Node b Red (Node c Red lllt llrt) lrt) rt) = 
                                        Node b col (Node c Black lllt llrt) (Node a Black lrt rt)
balance (Node a col (Node b Red llt (Node c Red lrlt lrrt)) rt) = 
                                        Node c col (Node b Black llt lrlt) (Node a Black lrrt rt) 
balance (Node a col lt (Node b Red (Node c Red rllt rlrt) rrt)) = 
                                        Node c col (Node a Black lt rllt) (Node b Black rlrt rrt)
balance (Node a col lt (Node b Red rlt (Node c Red rrlt rrrt))) = 
                                        Node b col (Node a Black lt rlt) (Node c Black rrlt rrrt)
balance s = blacken s 


{-





balanceDL :: RBTree a -> RBTree a 
balanceDL (Node a col (Node b Red (Node c Red lllt llrt) lrt) rt) = 
                                        Node b col (Node c Black lllt llrt) (Node a Black lrt rt)
balanceDL (Node a col (Node b Red llt (Node c Red lrlt lrrt)) rt) = 
                                        Node c col (Node b Black llt lrlt) (Node a Black lrrt rt) 
balanceDL s = blacken s 

balanceDR :: RBTree a -> RBTree a 
balanceDR (Node a col lt (Node b Red (Node c Red rllt rlrt) rrt)) = 
                                        Node c col (Node a Black lt rllt) (Node b Black rlrt rrt)
balanceDR (Node a col lt (Node b Red rlt (Node c Red rrlt rrrt))) = 
                                        Node b col (Node a Black lt rlt) (Node c Black rrlt rrrt)
balanceDR s = blacken s 
-}