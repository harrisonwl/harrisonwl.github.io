module Midterm4450Code where

import Prelude hiding (Either,Left,Right,Maybe,Just,Nothing)

f1 :: ((t1, t2) -> t) -> t1 -> t2 -> t
f1 f x y = f (x,y)

f2 :: (t1 -> t2 -> t) -> (t1, t2) -> t
f2 f (x,y) = f x y

f3 :: (t1 -> t2 -> t) -> t2 -> t1 -> t
f3 f x y = f y x

f4 :: [t] -> t
f4 [x,y] = x

--f5 :: (t1, t1) -> (t1, t1)
f5 :: (t1, t) -> (t1, t1)
f5 (x,y) = (x,x)

data Either a b = Left a | Right b
data Maybe a    = Just a | Nothing

f6 :: Either a t -> Maybe a
f6 (Left x)  = Just x
f6 (Right y) = Nothing

f7 :: Maybe a -> Either a b
f7 (Just x) = Left x
f7 Nothing  = Right undefined

f8 :: Either a t -> Either (Maybe a) (Maybe a1)
f8 (Left x)  = Left (Just x)
f8 (Right x) = Right Nothing

f9 :: (t1, t) -> (t, t1)
f9 (x,y) = (y,x)

f10 :: t -> t1 -> (t, t1)
f10 x = \ s -> (x,s)

lf1 :: [a] -> [a]
lf1 = map (\ x -> x)

lf2 :: a -> a
lf2 = \ x -> x

lf3 :: [a] -> [a]
lf3 []     = []
lf3 (x:xs) = x : lf3 xs

lf4 :: [a] -> [a]
lf4 = foldr (:) []

p1_1 :: (Int -> Int) -> Int -> Int
p1_1 _ _ = 9

p1_2 :: (Int -> Int) -> Int -> Int
p1_2 f x = 9

p1_3 :: (Int -> Int) -> Int -> Int
p1_3 f x = x

p1_4 :: (Int -> Int) -> Int -> Int
p1_4 f x = f (f (f x))
