module HW1 where

import Prelude hiding (filter)

{-
During the course of this semester, we'll be learning the functional programming
language Haskell as the vehicle for a broader study in the theory of programming
languages. In this assignment, we'll start by setting up Haskell and then
writing some simple functions.

Download and install the latest version of the Haskell platform from here:
   http://www.haskell.org/platform

What to submit (via Blackboard): a single file, named
HW1_pawprint.hs, where pawprint is your MU username. The file should
contain definitions for every function listed below. Furthermore,
everyone should adhere to the following guidelines to get full credit:

* Your submission must successfully load and typecheck in Haskell Platform to
get any points. For example, executing:
     $ ghci HW1_pawprint.hs
should not produce any errors. We won't attempt to grade assignments that fail to load.

* Name all functions and data types exactly as they appear in the
assignment. Grading will be partly automated, so incorrectly named functions are
likely to be counted as undefined functions.

* The code you submit must be your own. Exceptions: you may (of course) use
the code we provide however you like, including examples from the slides and the
book.

* No late submissions---PLEASE START EARLY!

-}

{-
For each of the following questions, put your answer directly below the 
question. This homework is due on Monday, September 12, by 3pm.

(1) You must use a text editor (e.g., vi, textpad, emacs, etc.) to prepare 
    your solution. 
(2) You must write type declarations for each and every one of your Haskell
    definitions.
(3) The program you turn in must be the product of your effort alone.

-}

{-
Define functions for arithmetic on complex numbers (e.g., usually written: 
a + bi). You must represent complex numbers in Haskell as pairs of Float
(i.e., (a,b)). If you haven't heard of complex numbers, check
out the wikipedia entry: http://en.wikipedia.org/wiki/Complex_number
-}

type Complex = (Float,Float)

-- 1. Define a function, magn, that computes the magnitude of a complex 
-- number. Recall that the magnitude of a complex number, a+bi, is defined
-- as: squareroot (a^2 + b^2). You can use the built-in Haskell function, sqrt,
-- in your answer. Put your definition below:


-- 2. Define a function, add, that takes two complex numbers and adds them.
-- Recall that (a + bi) + (c + di) = (a+c) + (b+d)i. 


-- 3. Define a function, sub, that subtracts two complex numbers.
-- Recall that (a + bi) - (c + di) = (a-c) + (b-d)i. 

-- 4. Define a function, mult, that multiplies two complex numbers.
-- Recall that (a + bi) * (c + di) =  (a*c - b*d) + (a*d + b*c)i

-- 5. Here is how a function called filter is defined in the Haskell
-- standard prelude:

filter :: (a -> Bool) -> [a] -> [a]
filter p []     = []
filter p (x:xs) = if p x 
                    then x : filter p xs 
                    else filter p xs

-- (filter p l) collects together the list elements of l on which p is true.
-- Ex. ghci> filter odd [1,2,3,4,5]
--     [1,3,5]
--
-- Write a function, sqrodd, that takes a list of Ints and returns the same 
-- list except that any odd number is squared. 
-- Ex. ghci> sqrodd [1,2,3,4,5]
--     [1,2,9,4,25]
-- You must write the function from "scratch" --- i.e., you can't use any 
-- other built-in Haskell function except odd. Hint: the code for sqrodd 
-- will look eerily similar to filter.

