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

-- Contructuor
node :: Ord a => a -> Colour -> RBTree a -> RBTree a -> RBTree a 
node a c x y = Node a c x y



-- |Inserts a new element into the correct place in the tree.
insert :: (Ord a) => RBTree a -> a -> RBTree a 
insert tree key = blacken (insert' tree) 
        where
        insert' Nil = Node key Red Nil Nil
        insert' t@(Node b c lt rt) | key < b = balanceL b c (insert' lt) rt 
                                   | key > b = balanceR b c lt (insert' rt)
                                   | otherwise = t -- ignore duplicate values   

balanceL :: Ord a => a -> Colour -> RBTree a -> RBTree a -> RBTree a 
balanceL a Black (Node b Red (Node c Red lllt llrt) lrt) rt = Node b Red (Node c Black lllt llrt) (Node a Black lrt rt)
balanceL a Black (Node b Red llt (Node c Red lrlt lrrt)) rt = Node c Red (Node b Black llt lrlt) (Node a Black lrrt rt)
balanceL a c lt rt = Node a c lt rt 

balanceR :: Ord a => a -> Colour -> RBTree a -> RBTree a -> RBTree a 
balanceR a Black lt (Node b Red rlt (Node c Red rrlt rrrt)) = Node b Red (Node a Black lt rlt) (Node c Black rrlt rrrt)
balanceR a Black lt (Node b Red (Node c Red rrlt rrrt) rrt) = Node c Red (Node a Black lt rrlt) (Node b Black rrrt rrt) 
balanceR a c lt rt = Node a c lt rt 

blacken :: RBTree a -> RBTree a 
blacken (Node a Red lt rt) = Node a Black lt rt 
blacken tree               = tree 

pattern Done :: a -> Either a b 
pattern Done x = Left x 

pattern Cont :: b -> Either a b 
pattern Cont x = Right x

apply :: (t -> b) -> Either t t -> Either b b
apply f (Done a) = Done (f a) 
apply f (Cont a) = Cont (f a)

fromEither :: Either a a -> a 
fromEither = either id id 

-- |Deletes an element from the tree, if it is present.
delete :: (Ord a) => RBTree a -> a -> RBTree a 
delete t key =  (blacken . fromEither . delete') t
    where delete' Nil = Done Nil 
          delete' t@(Node a col lt rt) | key < a   = apply (\lt' -> Node a col lt' rt) (delete' lt) >>= balHR  
                                       | key > a   = apply (\rt' -> Node a col lt rt') (delete' rt) >>= balHL  
                                       | otherwise = del t 

del :: Ord a => RBTree a -> Either (RBTree a) (RBTree a)
del (Node _ Red lt Nil)   = Done lt 
del (Node _ Black lt Nil) = blacken' lt  
del (Node _ col lt rt)    = apply (\rt' -> Node a col lt rt') tree >>= balHL 
                    where (tree, a) = successor rt 

successor :: Ord a => RBTree a -> (Either (RBTree a) (RBTree a), a)
successor (Node a Red Nil b)   = (Done b, a)
successor (Node a Black Nil b) = (blacken' b, a)
successor (Node a col lt rt)   = (apply (\lt' -> Node a col lt' rt) tree >>= balHR, suc)
                          where (tree, suc) = successor lt                                 

-- balance the height of the left subtree to match the right subtree
balHL :: Ord a => RBTree a -> Either (RBTree a) (RBTree a)
balHL (Node a col (Node b Black llt lrt) rt) = balanceL' (Node a col (Node b Red llt lrt) rt) 
balHL (Node a _ (Node b Red llt lrt) rt) = apply (\rt' -> Node b Black llt rt') (balHR (Node a Red lrt rt))


-- balance the height of the right subtree to match the left subtree
balHR :: Ord a => RBTree a -> Either (RBTree a) (RBTree a)
balHR (Node a col lt (Node b Black rlt rrt)) = balanceR' (Node a col lt (Node b Red rlt rrt))
balHR (Node a _ lt (Node b Red rlt rrt)) = apply (\lt' -> Node b Black lt' rrt) (balHR (Node a Red lt rlt))


-- Similar to insertion, Tries to correct the height deficit if possible otherwise bubbles the deficit upwards
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


blacken' ::Ord a => RBTree a -> Either (RBTree a) (RBTree a)
blacken' (Node a Red lt rt) = Done $ Node a Black lt rt 
blacken' tree               = Cont tree 


-- |Returns true if the tree contains the given element.
contains :: (Ord a) => RBTree a -> a -> Bool 
contains Nil _ =  False
contains (Node a _ lt rt) key | a > key   = contains lt key 
                              | a < key   = contains rt key 
                              | otherwise = True  
