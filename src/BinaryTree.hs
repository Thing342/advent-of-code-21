module BinaryTree where

data BinaryTree a = Stem (BinaryTree a) a (BinaryTree a) 
    | Leaf 
    deriving (Show)

ascending :: (Ord a) => BinaryTree a -> [a]
ascending (Leaf) = []
ascending (Stem left val right) = (ascending left) ++ [val] ++ (ascending right)

descending :: (Ord a) => BinaryTree a -> [a]
descending (Leaf) = []
descending (Stem left val right) = (descending right) ++ [val] ++ (descending left)

insert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert x Leaf = Stem Leaf x Leaf
insert x (Stem left y right)
    | x <= y = Stem (insert x left) y right
    | otherwise = Stem left y (insert x right)

fromList :: (Ord a) => [a] -> BinaryTree a
fromList = foldr insert Leaf

treeMedian :: (Ord a) => BinaryTree a -> Either (a,a) a
treeMedian tree = let
    b = (ascending tree) `zip` (descending tree)
    (lo,hi) = head $ dropWhile (\(lo,hi) -> lo < hi) b
    in if lo == hi then Right lo else Left (lo, hi)