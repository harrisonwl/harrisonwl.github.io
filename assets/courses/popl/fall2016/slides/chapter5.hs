module Chapter5 where

import Prelude hiding (maximum,replicate,take,repeat,zip,elem)

{-
ghci> maximum [1,101,3,-9]
101
ghci> maximum []
*** Exception: Prelude.maximum: empty list

maximum :: (Ord a) => [a] -> a
maximum []     = error "Ugghh."
maximum (n:[]) = n
maximum (n:ns) = if n <= m then m else n
  where m = maximum ns

-}

maximum :: (Ord a) => [a] -> a
maximum []     = error "Ugghh."
maximum (n:ns) | null ns   = n
               | otherwise = if n <= m then m else n
  where m = maximum ns

{-
ghci> replicate 7 'a'
"aaaaaaa"
ghci> replicate 0 'a'
""
ghci> replicate (-1) 'a'
""
-}

--replicate :: Int -> a -> [a]
replicate :: (Num a, Ord a) => a -> t -> [t]
replicate n x | n <= 0 = []
              | otherwise = x : replicate (n-1) x

{-
ghci> take 3 [1,2,3,4,5]
[1,2,3]
ghci> take 0 [1,2,3,4,5]
[]
ghci> take 3 []
[]
ghci> take (-1) [1,2,3,4,5]
[]
-}

take :: Int -> [a] -> [a]
take n xs | n<=0 || null xs = []
          | otherwise       = head xs : take (n-1) (tail xs)

take' _ []     = []
take' 0 _      = []
take' n (x:xs) = x : take' (n-1) xs

{-
ghci> take 7 (repeat 'a')
"aaaaaaa"
ghci> take 40 (repeat 'a')
"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
-}

repeat :: a -> [a]
repeat x = x : repeat x

{-
ghci> zip [1,2,3] ['a','b','c']
[(1,'a'),(2,'b'),(3,'c')]
ghci> zip [1,2,3] ['a','b']
[(1,'a'),(2,'b')]
ghci> zip [] ['a','b']
[]
-}

zip :: [a] -> [b] -> [(a,b)]
zip [] _          = []
zip _ []          = []
zip (y:ys) (x:xs) = (y,x) : zip ys xs

{-
ghci> elem 1 [4,5,6,1]
True
ghci> elem 'a' "haskell"
True
ghci> elem 'i' "team"
False
-}

elem :: Eq a => a -> [a] -> Bool
elem x []     = False
elem x (y:ys) = if x==y then True else elem x ys

elem' :: Eq a => a -> [a] -> Bool
elem' x []     = False
elem' x (y:ys) | x==y      = True
               | otherwise = elem x ys
