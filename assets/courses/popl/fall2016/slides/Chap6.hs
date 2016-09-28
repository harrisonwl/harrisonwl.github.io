module Chap6 where

import Prelude hiding (map,filter,foldr,sum,foldl,reverse)

iadd :: [Int] -> Int
iadd []     = 0
iadd (x:xs) = x + iadd xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr oper d []     = d
foldr oper d (x:xs) = x `oper` foldr oper d xs

map, map', map'' :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = (f x) : (map f xs)

map' f = foldr oper []
  where oper head recresult = (f head) : recresult

map'' f = foldr (\ head recresult -> (f head) : recresult) []

filter, filter', filter'' :: (a -> Bool) -> [a] -> [a]
filter p []     = []
filter p (x:xs) = if p x then x : (filter p xs) else filter p xs

filter' p = foldr oper []
  where oper head recresult = if p head then head : recresult else recresult

filter'' p
  = foldr (\ head recresult -> if p head then head : recresult else recresult) []


sum, sum', sum'' :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs

sum' = foldr oper 0
  where oper head recresult = head + recresult

sum'' = foldr (\ head recresult -> head + recresult) 0


foldl f acc []     = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

summacc :: Int -> [Int] -> Int
summacc acc []     = acc
summacc acc (x:xs) = summacc (acc + x) xs

sum4 = foldl oper 0
  where oper head acc = acc + head

sum''' = foldr (+) 0
sum5 = foldl (+) 0

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

reverse' = foldr oper []
  where oper head recresult = recresult ++ [head]

reverse'' = foldr (\ head recresult -> recresult ++ [head]) []

revacc acc []     = acc
revacc acc (x:xs) = revacc (x:acc) xs

revacc' = foldl oper []
  where oper head acc = head : acc

revacc'' = foldl (\ head acc -> head : acc) []

revacc''' = foldl (:) []

