module HW2 where

import Prelude hiding (sum)


{-
What to submit (via Blackboard): a single file, named
HW2_pawprint.hs, where pawprint is your MU username. The file should
contain definitions for every function listed below. Furthermore,
everyone should adhere to the following guidelines to get full credit:

* Your submission must successfully load and typecheck in Haskell Platform to
get any points. For example, executing:
     $ ghci HW2_pawprint.hs
should not produce any errors. We won't attempt to grade assignments that fail to load.

* Name all functions and data types exactly as they appear in the
assignment. Grading will be partly automated, so incorrectly named functions are
likely to be counted as undefined functions.

* The code you submit must be your own. Exceptions: you may (of course) use
the code we provide however you like, including examples from the slides.

* No late submissions---PLEASE START EARLY!

-}

{-
For each of the following questions, put your answer directly below the 
question. This homework is due on Monday, September 26, by 3pm.

(1) You must use a text editor (e.g., vi, textpad, emacs, etc.) to prepare 
    your solution. 
(2) You must write type declarations for each and every one of your Haskell
    definitions.
(3) The program you turn in must be the product of your effort alone.
(4) You may *not* import any Haskell library without the expressed permission
    of Professor Harrison. In other words, do not add any import declarations at
    the top of this file.
-}


{-
Problem 1.
Using pattern-matching with (:), define a function rev2 that reverses all
lists of length 2, but leaves others unchanged. Ensure that your solution
works for all lists --- that is, that the patterns you use are exhaustive.
-}

prob1 = undefined

{-
Problem 2.
Define exponentiation (usually written x^n) as a list comprehension.
-}

prob2 = undefined

{-
Problem 3.
Write the function snoc that takes an item x and a list of items xs and
returns the list of items with x  inserted at the end of xs. So, for example,
snoc 9 [1,2,3] is [1,2,3,9]. To receive credit, your solution must only
use recursion and cons (:) --- in particular, you may not use append (++).
-}

prob3 = undefined

{-
Problem 4.

In class, we wrote a function in "accumulator passing style" (APS) which reverses a list. The
code for this function is below:
-}

rev :: [a] -> [a]
rev xs = rev' [] xs
   where
     rev' :: [a] -> [a] -> [a]
     rev' acc []     = acc
     rev' acc (x:xs) = rev' (x:acc) xs

{-
In the definition above, the function, rev', is in APS. Below is the prelude function, sum. 
-}

sum :: Num a => [a] -> a
sum []     = 0
sum (n:ns) = n + sum ns

{-
For problem 4, write a function in APS that sums a list of numbers.
-}

prob4 = undefined

{-
Problem 5.
For problem 5, write a function in APS which, when given a non-empty list of non-negative numbers, returns
the maximum number in the list. When given an empty list, it should return 0.
-}

prob5 = undefined
