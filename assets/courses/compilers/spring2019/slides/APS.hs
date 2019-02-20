module APS where

import Prelude hiding (reverse,foldl,(++))

import Debug.Trace

-- λ> :t trace
-- trace :: String -> a -> a

(++) :: [a] -> [a] -> [a]
[] ++ ys     = ys
(x:xs) ++ ys = x : (xs ++ ys)

reversebad []     = []
reversebad (x:xs) = reversebad xs ++ [x]

--
-- Accumulator Passing Style (APS).
--

reverse = revacc []

revacc :: [a] -> [a] -> [a]
revacc acc []     = acc
revacc acc (x:xs) = revacc (x:acc) xs

--
-- APS is just a left fold.
--

foldl :: (acc -> a -> acc) -> acc -> [a] -> acc
foldl f acc []     = undefined
foldl f acc (a:as) = undefined

revacc' acc xs = undefined
