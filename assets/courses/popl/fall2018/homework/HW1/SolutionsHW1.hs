module SolutionsHW1 where

import Prelude hiding (filter)

{-
During the course of this semester, we'll be learning the functional programming
language Haskell as the vehicle for a broader study in the theory of programming
languages. In this assignment, we'll start by setting up Haskell and then
writing some simple functions.

Download and install the latest version of the Haskell platform from here:
   http://www.haskell.org/platform

What to submit (via Canvas): a single file, named
HW1_pawprint.hs, where pawprint is your MU username. The file should
contain definitions for every function listed below.

Furthermore, everyone should adhere to the following guidelines to get full credit:

* Your submission must successfully load and typecheck in Haskell Platform to
get any points. For example, executing:
     $ ghci HW1_pawprint.hs
should not produce any errors. We won't attempt to grade assignments that fail to load,
meaning you'll get zero points for this homework.

* Name all functions and data types exactly as they appear in the
assignment. Grading will be partly automated, so incorrectly named functions are
likely to be counted as undefined functions.

* The code you submit must be your own. Exceptions: you may (of course) use
the code we provide however you like, including examples from the slides and the
books.

* No late submissions---PLEASE START EARLY!

-}

{-
For each of the following questions, put your answer directly below the 
question. This homework is due on Monday, September 11, by 3pm.

(1) You must use a text editor (e.g., vi, textpad, emacs, etc.) to prepare 
    your solution. 
(2) You must write type declarations for each and every one of your Haskell
    definitions.
(3) The program you turn in must be the product of your effort alone.

There are ten questions, each worth 10 points, making this homework
worth 100 points total. Some are harder than others.

-}

{-
Define functions for arithmetic on complex numbers (e.g., usually written: 
a + bi). You must represent complex numbers in Haskell as pairs of Float
(i.e., (a,b)). If you haven't heard of complex numbers, check
out the wikipedia entry: http://en.wikipedia.org/wiki/Complex_number
-}

type Complex = (Float,Float)

-- Problem 1. Define a function, magn, that computes the magnitude of a complex 
-- number. Recall that the magnitude of a complex number, a+bi, is defined
-- as: squareroot (a^2 + b^2). You can use the built-in Haskell function, sqrt,
-- in your answer. Put your definition below:

magn :: Complex -> Float
magn (a,b) = sqrt (a^2 + b^2)

-- Problem 2. Define a function, add, that takes two complex numbers and adds them.
-- Recall that (a + bi) + (c + di) = (a+c) + (b+d)i. 

add :: Complex -> Complex -> Complex
add (a,b) (c,d) = (a+c,b+d)

-- Problem 3. Define a function, sub, that subtracts two complex numbers.
-- Recall that (a + bi) - (c + di) = (a-c) + (b-d)i. 

sub :: Complex -> Complex -> Complex
sub (a,b) (c,d) = (a-c,b-d)

-- Problem 4. Define a function, mult, that multiplies two complex numbers.
-- Recall that (a + bi) * (c + di) =  (a*c - b*d) + (a*d + b*c)i

mult :: Complex -> Complex -> Complex
mult (a,b) (c,d) = (a*c - b*d, a*d + b*c)

-- Problem 5. Here is how a function called filter is defined in the Haskell
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
-- Note that the function "odd" is built-in to Haskell.
--
-- Write a function, sqrodd, that takes a list of Ints and returns the same 
-- list except that any odd number is squared. 
-- Ex. ghci> sqrodd [1,2,3,4,5]
--     [1,2,9,4,25]
-- You must write the function from "scratch" --- i.e., you can't use any 
-- other built-in Haskell function except odd. Hint: the code for sqrodd 
-- will look eerily similar to filter.

sqrodd :: [Int] -> [Int]
sqrodd []     = []
sqrodd (x:xs) = if odd x
                  then (x^2) : sqrodd xs
                  else x : sqrodd xs
                  
-----------------------------------------------------------------
-----------------------------------------------------------------
-----------------------------------------------------------------

-- Recall from class the abstract syntax for propositional logic and
-- its expression as a Haskell data type:

type Var = String
data Prop = Atom Var 
          | Not Prop
          | Imply Prop Prop
          | And Prop Prop   -- this is new

-- Terminology. We say that (And p q) is the "conjunction" of p and q.
            
-- Recall, we can represent (- p) as the following Haskell value:

negp :: Prop
negp = Not (Atom "p")

-- Problem 6. Define "(a => (b => c)) => ((a & b) => c)" using the above
-- abstract syntax. Write your answer

prob6 :: Prop
prob6 = Imply pre post
  where
    pre  = Imply a (Imply b c)
    post = Imply (And a b) c
    a    = Atom "a"
    b    = Atom "b"
    c    = Atom "c"
    -- you don't have to use a where clause, it just makes it simpler.
    
-- Below are the Show and Eq instances for Prop that we gave in class.

-- Problem 7. Define show for And below, using ampersand (i.e., '&') for And.
-- Check your answer to this question against your answer to Problem 6.
instance Show Prop where
  show (Atom p)            = p
  show (Not prop)          = "(-" ++ show prop ++ ")"
  show (Imply prop1 prop2) = "(" ++ show prop1 ++ " => " ++ show prop2 ++ ")"
  show (And prop1 prop2)   = "(" ++ show prop1 ++ " & " ++ show prop2 ++ ")"

-- Problem 8. Define == for And below.
instance Eq Prop where
  (Atom p) == (Atom q)       = p == q
  (Not x) == (Not y)         = x == y
  (Imply x y) == (Imply u v) = (x == u) && (y == v)
  (And x y) == (And u v)     = (x == u) && (y == v)
  _ == _                     = False


--Problem 9. 

-- The next question concerns writing a definitional extension of the Prop language
-- as defined above. See "Defined Connectives as Functions" in the SyntaxVsSemantics slides
-- for background explanation. The "Sheffer stroke" (https://en.wikipedia.org/wiki/Sheffer_stroke)
-- for propositional sentences p and q can be defined as the negation of the conjunction of
-- p and q. What you will do is analogous to what we did in class with the "orPL", "andPL", and
-- "iffPL" functions (see the aforementioned slides).

sheffer :: Prop -> Prop -> Prop
sheffer p q = Not (And p q)

-- Problem 10.

-- The next question concerns writing a function that collects all of the propositional
-- variables in a Prop. You will write a function, vars, that takes a Prop as input and
-- produces a list of String as output. The type template for this function is below.

-- Review "HaskellForGrownups" slides and, in particular, "How to Write a Haskell Function"
-- slides. The smart way to do this problem is in a stepwise fashion. That is, (1) replace
-- a single undefined with what you think the correct definition is and, then, (2) load that
-- into ghci to see if it typechecks. Your answer may produce repeated entries. For example,
-- evaluating (vars prob6) with ghci may return ["a","b","c","a","b","c"].

vars :: Prop -> [String]
vars (Atom p)      = [p]
vars (Not p)       = vars p
vars (Imply p1 p2) = vars p1 ++ vars p2
vars (And p1 p2)   = vars p1 ++ vars p2
