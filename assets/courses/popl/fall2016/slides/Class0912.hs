module Class0912 where

--
-- 1. Find the last element of a list.
--

prob1 :: [a] -> a
prob1 []     = error "list is empty, stupid."
prob1 (x:xs) = case xs of
                    [] -> x
                    _  -> prob1 xs
               
{-
prob1 :: [a] -> a
prob1 []     = error "list is empty, stupid."
prob1 (x:xs) | null xs   = x
             | otherwise = prob1 xs

prob1 :: [a] -> a
prob1 []     = error "list is empty, stupid."
prob1 (x:xs) = if null xs then x else prob1 xs

prob1 :: [a] -> a
prob1 []     = error "list is empty, stupid."
prob1 (x:[]) = x
prob1 (_:xs) = prob1 xs
-}
               
  -- ``if xs is [] then x else prob1 xs''

-- data [a] = [] | (a : [a])

{-
Prelude> prob1 [1,2,3,4]
4
Prelude> prob1 ['x','y','z']
'z'
-}

--
-- 2. Find the last but one element of a list.
--

prob2 :: [a] -> a
prob2 []         = error "duh"
prob2 (x:[])     = error "double duh"
prob2 (x:(y:[])) = x
prob2 (x:(y:xs)) = prob2 (y:xs)

{-
Prelude> prob2 [1,2,3,4]
3
Prelude> prob2 ['a'..'z']
'y'
-}

--
-- 3. Find the K'th element of a list. 
--

prob3 = undefined
{-
Prelude> prob3 [1,2,3] 2
2
Prelude> prob3 "haskell" 5
'e'
-}

--
-- 4. Find the number of elements of a list.
--

prob4 = undefined
{-
Prelude> prob4 [123, 456, 789]
3
Prelude> prob4 "Hello, world!"
13
-}

--
-- 5. Reverse a list.
--

prob5 = undefined
{-
Prelude> prob5 "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
Prelude> prob5 [1,2,3,4]
[4,3,2,1]
-}

--
-- 6. Find out whether a list is a palindrome. A palindrome
--    can be read forward or backward; e.g. "xamax".
--

prob6 = undefined
{-
*Main> prob6 [1,2,3]
False
*Main> prob6 "madamimadam"
True
*Main> prob6 [1,2,4,8,16,8,4,2,1]
True
-}


--
-- 7. Flatten a nested list structure.
--

-- We have to define a new data type, because lists in Haskell are homogeneous.

data NestedList a = Elem a | List [NestedList a]

prob7 = undefined
{-
*Main> prob7 (Elem 5)
[5]
*Main> prob7 (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
[1,2,3,4,5]
*Main> prob7 (List [])
[]
-}


--
-- 8. Eliminate consecutive duplicates of list elements.
--

prob8 = undefined
{-
> prob8 "aaaabccaadeeee"
"abcade"
-}


--
-- 9. Pack consecutive duplicates of list elements into sublists.
--    If a list contains repeated elements they should be placed
--    in separate sublists.
--

prob9 = undefined
{-
*Main> prob9 ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e']
["aaaa","b","cc","aa","d","eeee"]
-}


--
-- 10. Run-length encoding of a list. Use the result of problem 9 to
--     implement the so-called run-length encoding data compression 
--     method. Consecutive duplicates of elements are encoded as lists 
--     (N E) where N is the number of duplicates of the element E.
--

prob10 = undefined
{-
prob10 "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-}


--
-- 11. Modified run-length encoding. Modify the result of problem 10
--     in such a way that if an element has no duplicates it is simply
--     copied into the result list. Only elements with duplicates are
--     transferred as (N E) lists.
--

prob11 = undefined
{-
P11> prob11 "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

