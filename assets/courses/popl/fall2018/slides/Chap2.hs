module Chap2 where

import Prelude hiding (head,tail,(!!),last,take,reverse)

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

-- We'll add to this on Friday.
