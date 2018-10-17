module Chap6 where

import Prelude hiding (foldr,length)

iadd :: [Int] -> Int
iadd []     = 0
iadd (x:xs) = x + iadd xs

iadd' = foldr (+) 0

length = foldr (\ _ v -> 1 + v) 0

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f d []     = d
foldr f d (x:xs) = x `f` (foldr f d xs)
