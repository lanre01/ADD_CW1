{-# LANGUAGE PatternSynonyms #-}

module RedBlackTrees (
    Colour (..),
    RBTree (..),
    fromList,
    isBST,
    isRBT,
    insert,
    delete,
    contains,
) where


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

-- Constructs a list from a RBTree
toList :: Ord a => RBTree a -> [a]
toList = foldr (:) []

-- |Returns true if the argument is a valid binary search tree.
isBST :: (Ord a) => RBTree a -> Bool
isBST = (orderedBy (<=)) . toList

orderedBy :: (a -> a -> Bool) -> [a] -> Bool
orderedBy _ []       = True
orderedBy _ [_]      = True
orderedBy f (x:y:xs) = f x y && orderedBy f (y:xs)


-- |Returns true if the argument is a valid red-black tree.
isRBT :: (Ord a) => RBTree a -> Bool 
isRBT t = isBlackNode t && allChecks t Nothing Nothing /= Nothing

-- Single-pass red-black validation: bounds, red-red rule, and black-height.
allChecks :: Ord a => RBTree a -> Maybe a -> Maybe a -> Maybe Int
allChecks Nil _ _ = Just 1  
allChecks (Node x c lt rt) lo hi = do
                            withinBounds lo hi x 
                            redInvariant c lt rt 
                            hl <- allChecks lt lo (Just x)
                            hr <- allChecks rt (Just x) hi

                            if hl == hr then return (hl + if isBlack c then 1 else 0) 
                            else Nothing

withinBounds :: Ord a => Maybe a -> Maybe a -> a -> Maybe ()
withinBounds Nothing  Nothing  _ = Just ()
withinBounds (Just lo) Nothing v = if lo < v then pure () else Nothing
withinBounds Nothing  (Just hi) v = if v < hi then pure () else Nothing
withinBounds (Just lo) (Just hi) v = if lo < v && v < hi then pure () else Nothing

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

-- The implementation uses `Either` as a control-flow monad.
-- `Done` wraps `Left` to mean “stop / no further fix-up needed”, while
-- `Continue` wraps `Right` to mean “keep bubbling the fix-up upward”.
-- This avoids defining a custom wrapper type and writing a new `Monad`
-- instance solely to encode this two-state behaviour.
pattern Done :: a -> Either a b 
pattern Done x = Left x 

pattern Continue :: b -> Either a b 
pattern Continue x = Right x

{-# COMPLETE Done, Continue #-}

-- injects a function into an either Left or Right constructor
inject :: (t -> b) -> Either t t -> Either b b
inject f (Done a) = Done (f a) 
inject f (Continue a) = Continue (f a)

fromEither :: Either a a -> a 
fromEither = either id id 

-- constructs a new node which is always red
node :: Ord a => a -> RBTree a 
node a = Node a Red Nil Nil 

-- |Inserts a new element into the correct place in the tree.
insert :: (Ord a) => RBTree a -> a -> RBTree a 
insert tree key = (blacken . fromEither . insert') tree 
        where
        insert' Nil = Continue $ node key
        insert' t@(Node b c lt rt) | key < b = inject (\lt' -> Node b c lt' rt) (insert' lt) >>= balanceL  
                                   | key > b = inject (\rt' -> Node b c lt rt') (insert' rt) >>= balanceR
                                   | otherwise = Done t -- ignores duplicate values   

-- fixes red-red violations
balanceL :: Ord a => RBTree a -> Either (RBTree a) (RBTree a)
balanceL (Node a Black (Node b Red (Node c Red lllt llrt) lrt) rt) = 
                            Continue $ Node b Red (Node c Black lllt llrt) (Node a Black lrt rt)
balanceL (Node a Black (Node b Red llt (Node c Red lrlt lrrt)) rt) = 
                            Continue $ Node c Red (Node b Black llt lrlt) (Node a Black lrrt rt)
balanceL t@(Node _ Black _ _) = Done t 
balanceL t                    = Continue t  

balanceR :: Ord a => RBTree a -> Either (RBTree a) (RBTree a)
balanceR (Node a Black lt (Node b Red rlt (Node c Red rrlt rrrt))) = 
                            Continue $ Node b Red (Node a Black lt rlt) (Node c Black rrlt rrrt)
balanceR (Node a Black lt (Node b Red (Node c Red rrlt rrrt) rrt)) = 
                            Continue $ Node c Red (Node a Black lt rrlt) (Node b Black rrrt rrt) 
balanceR t@(Node _ Black _ _) = Done t 
balanceR t                    = Continue t

blacken :: RBTree a -> RBTree a 
blacken (Node a Red lt rt) = Node a Black lt rt 
blacken tree               = tree 

blacken' ::Ord a => RBTree a -> Either (RBTree a) (RBTree a)
blacken' (Node a Red lt rt) = Done $ Node a Black lt rt 
blacken' tree               = Continue tree 


-- |Deletes an element from the tree, if it is present.
delete :: (Ord a) => RBTree a -> a -> RBTree a 
delete tree key =  (blacken . fromEither . delete') tree
    where delete' Nil = Done Nil 
          delete' t@(Node a col lt rt) | key < a   = inject (\lt' -> Node a col lt' rt) (delete' lt) >>= balHR  
                                       | key > a   = inject (\rt' -> Node a col lt rt') (delete' rt) >>= balHL  
                                       | otherwise = del t 

del :: Ord a => RBTree a -> Either (RBTree a) (RBTree a)
del Nil                   = error "Nothing to delete"
del (Node _ Red lt Nil)   = Done lt 
del (Node _ Black lt Nil) = blacken' lt  
del (Node _ col lt rt)    = inject (\rt' -> Node suc col lt rt') tree >>= balHL 
                    where (tree, suc) = (successor rt) 

successor :: Ord a => RBTree a -> (Either (RBTree a) (RBTree a), a)
successor Nil                   = error "No successor from Nil node"
successor (Node a Red Nil rt)   = (Done rt, a)
successor (Node a Black Nil rt) = (blacken' rt, a)
successor (Node a col lt rt)    = (inject (\lt' -> Node a col lt' rt) eTree >>= balHR, suc)
                          where (eTree, suc) = successor lt                                 

-- balance the black height of the left subtree to match the right subtree
balHL :: Ord a => RBTree a -> Either (RBTree a) (RBTree a)
balHL (Node a col (Node b Black llt lrt) rt) = balanceL' (Node a col (Node b Red llt lrt) rt) 
balHL (Node a _ (Node b Red llt lrt) rt)     = inject (\rt' -> Node b Black llt rt') 
                                                      (balHL (Node a Red lrt rt))
balHL                     _              = error "No black height deficit possible"

-- balance the black height of the right subtree to match the left subtree
balHR :: Ord a => RBTree a -> Either (RBTree a) (RBTree a)
balHR (Node a col lt (Node b Black rlt rrt)) = balanceR' (Node a col lt (Node b Red rlt rrt))
balHR (Node a _ lt (Node b Red rlt rrt))     = inject (\lt' -> Node b Black lt' rrt) 
                                                      (balHR (Node a Red lt rlt))
balHR                     _                  = error "No black height deficit possible"

-- Tries to correct the height deficit if possible otherwise bubbles the deficit upwards
balanceL' :: Ord a => RBTree a -> Either (RBTree a) (RBTree a)
balanceL' (Node a col (Node b Red (Node c Red lllt llrt) lrt) rt) = 
                                      Done $ Node b col (Node c Black lllt llrt) (Node a Black lrt rt)
balanceL' (Node a col (Node b Red llt (Node c Red lrlt lrrt)) rt) = 
                                      Done $ Node c col (Node b Black llt lrlt) (Node a Black lrrt rt)
balanceL' tree = blacken' tree  

balanceR' :: Ord a => RBTree a -> Either (RBTree a) (RBTree a)
balanceR' (Node a col lt (Node b Red (Node c Red rllt rlrt) rrt)) = 
                             Done $ Node c col (Node a Black lt rllt) (Node b Black rlrt rrt) 
balanceR' (Node a col lt (Node b Red rlt (Node c Red rrlt rrrt))) = 
                             Done $ Node b col (Node a Black lt rlt) (Node c Black rrlt rrrt)
balanceR' tree = blacken' tree





-- |Returns true if the tree contains the given element.
contains :: (Ord a) => RBTree a -> a -> Bool 
contains Nil _ =  False
contains (Node a _ lt rt) key | a > key   = contains lt key 
                              | a < key   = contains rt key 
                              | otherwise = True  
