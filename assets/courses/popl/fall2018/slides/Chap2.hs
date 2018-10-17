module Chap2 where

import Prelude hiding (head,tail,(!!),last,take,reverse,drop,maximum,repeat,zip,cycle)

doubleMe x = x + x  

data Horf = Oggg Int
          | Smorg Horf Horf
          | Lerm Horf Horf
          | Rogg Horf Horf

instance Eq Horf where
  (Oggg i) == (Oggg j)       = i==j
  (Smorg p q) == (Smorg u v) = p==u && q==v
  (Lerm p q) == (Lerm u v)   = p==u && q==v
  (Rogg p q) == (Rogg u v)   = p==u && q==v
  _ == _                     = False

head :: [a] -> a
head []    = error "Hey stupid, it's empty"
head (x:_) = x

tail :: [a] -> [a]
tail []     = error "Hey stupid, it's empty"
tail (_:xs) = xs

(!!) :: [a] -> Int -> a
[] !! i     = error "empty!"
--(x:xs) !! i = if i == 0 then x else xs !! (i-1)
(x:xs) !! i | i==0      = x
            | i>0       = xs !! (i-1)
            | otherwise = error "bleeehhhh empty"

last :: [a] -> a
last []     = error "blehh"
-- last (x:[]) = x
-- last (x:xs) = last xs
last (x:xs) = case xs of
  [] -> x
  _  -> last xs

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

take :: Int -> [a] -> [a]
take 0 _      = []
take n []     = []
take n (x:xs) = x : take (n-1) xs

-- an alternative
take' :: Int -> [a] -> [a]
take' n l | n<=0 = [] -- N.b., defined also for n<0
          | n>0  = case l of
                    []     -> []
                    (x:xs) -> x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop 0 ls = ls
drop n []     = []
drop n (x:xs) = drop (n-1) xs

maximum :: [Int] -> Int
maximum []     = 0
maximum (x:xs) = if x > maxxs then x else maxxs
   where maxxs = maximum xs

         
repeat :: a -> [a]
repeat x = x : repeat x

fives = repeat 5

cycle :: [a] -> [a]
cycle l = l ++ cycle l

zip :: [a] -> [b] -> [(a,b)]
zip [] _          = []
zip _ []          = []
zip (a:as) (b:bs) = (a,b) : zip as bs
