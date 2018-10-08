module Tree where

data Tree = Leaf Int | Branch Tree Tree
  deriving Show

t = Branch (Branch (Leaf 5) (Leaf 3)) (Leaf (-99))

minTree (Leaf n) = n
minTree (Branch a b) = min (minTree a) (minTree b)

maxTree (Leaf n) = n
maxTree (Branch a b) = max (maxTree a) (maxTree b)
           
minMax (Leaf n) = (n, n)
minMax (Branch a b) = (min min1 min2, max max1 max2)
  where (min1, max1) = minMax a
        (min2, max2) = minMax b
