module Bootcamp where

data Answer = Yes | No | Unknown deriving (Eq)

data Shape = Circle Float | Rect Float Float deriving (Eq)

square :: Float -> Shape
square x = Rect x x

area :: Shape -> Float
area (Circle x) = 3.14 * x * x
area (Rect x y) = x * y

foo (Circle f) = undefined
foo (Rect x y) = undefined


instance Show Shape where
  show (Circle x) = "Circle " ++ show x ++ " ; area = " 
                         ++ show (area (Circle x))
  show (Rect x y) = "Rect " ++ show x ++ " " ++ show y

-- data Maybe a = Nothing | Just a
  
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

data Nat = Zero | Succ Nat  

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

v = Succ (Succ (Succ (Succ Zero)))

-- (==) :: Nat -> Nat -> Bool
-- show :: Nat -> String

instance Show Nat where
  show Zero     = "0"
  show (Succ n) = "1 + " ++ show n

instance Eq Nat where
  Zero == Zero         = True
  (Succ n) == Zero     = False
  Zero == (Succ n)     = False
  (Succ n) == (Succ m) = n == m

add :: Nat -> Nat -> Nat  
add n Zero     = n
--add n (Succ m) = add (Succ n) m
add n (Succ m) = Succ (add n m)


data Expr = Val Int | Add Expr Expr | Mul Expr Expr deriving Show

eval :: Expr -> Int
eval (Val n)     = n
eval (Add e1 e2) = eval e1 + eval e2 
eval (Mul e1 e2) = eval e1 * eval e2 

data Trg = Push Int | Pop | ADD | MUL
data Stk = Empty | Ins Int Stk deriving Show

exec :: Trg -> Stk -> Stk
exec (Push i) s = Ins i s
exec Pop s      = case s of
                       Empty       -> error "Ackk, empty stack"
                       (Ins j stk) -> stk
exec ADD s      = case s of
                       Empty               -> error "Add hates you"
                       (Ins j (Ins n stk)) -> Ins (j+n) stk
                       _                   -> error "Add hates you"
exec MUL s      = case s of
                       Empty               -> error "Mul hates you"
                       (Ins j (Ins n stk)) -> Ins (j*n) stk
                       _                   -> error "Mul hates you"


execl :: [Trg] -> Stk -> Stk
execl [] s = s
execl (c:cs) s = execl cs (exec c s)

{-
> comp e
[push 1,push 2,push 3,mul,add]
*Bootcamp> execl (comp e) Empty
Ins 7 Empty
*Bootcamp> 
-}

instance Show Trg where
   show (Push i) = "push " ++ show i
   show Pop      = "pop"
   show ADD      = "add"
   show MUL      = "mul"

showTrg :: Trg -> String
showTrg (Push i) = "push " ++ show i
showTrg Pop      = "pop"
showTrg ADD      = "add"
showTrg MUL      = "mul"
  
comp :: Expr -> [Trg]
comp (Val n)     = [Push n]
comp (Add e1 e2) = c1 ++ c2 ++ [ADD]
  where c1 = comp e1
        c2 = comp e2
comp (Mul e1 e2) = c1 ++ c2 ++ [MUL]
  where c1 = comp e1
        c2 = comp e2

e = Add (Val 1) (Mul (Val 2) (Val 3))