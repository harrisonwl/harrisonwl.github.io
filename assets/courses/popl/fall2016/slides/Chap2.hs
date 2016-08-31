module Chap2 where

import Prelude hiding (head,tail,(!!))

doubleMe x = x + x  

head :: [a] -> a
head []     = error "Hey Stupid it's empty"
head (x:xs) = x

tail :: [a] -> [a]
tail []     = undefined
tail (x:xs) = xs

-- (!!) :: [a] -> Int -> a
[] !! n     = undefined
(x:xs) !! n = if n==0 then x else xs !! (n-1)


