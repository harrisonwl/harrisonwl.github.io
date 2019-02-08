module HW1 where

{-
10 Questions/ 10 points each.

Due Tuesday, February 12th by 11:59pm.

General Requirements.
---------------------
1. Your solutions to these questions are to be included in this file only.
2. Turn in your homework on blackboard only. Emailing your answers to me or 
   the TAs or turning in a print out in class is the same as not turning it
   in.
3. IMPORTANT: your homework must load into ghci without errors to receive
   any credit. That is, if what you turn in does not load into ghci
   then you will receive a zero for this assignment. No exceptions.
4. If you don't finish a problem and have something that you'd like to have
   considered as partial credit, put it within a comment. This isn't a 
   guarantee that you'll get partial credit, though.
5. Type declarations must be included with every definition you give.

Turning In Your Solution.
-------------------------
To turn in your solution, please email me the code (harrisonwl@missouri.edu)
with the subject "CS4430 HW1". It is important that you get this small detail
right because otherwise I may miss your submitted solution. Your solution
should be in the form of a single Haskell file named "Last-name_HW1.hs".
So, if I did this homework, I’d turn in "Harrison_HW1.hs".

-}

{-
Background Material: Chapters 3 & 4, Learn You a Haskell for Great Good.
Advice:              Remember the type template idea as you proceed.
-}

data Nat = Zero | Succ Nat

{- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
1. The "type template" idea was discussed repeatedly in CS4450/7450. It is an 
extremely helpful way to get started writing a Haskell function of a particular
type. For instance, let's say I'm writing a function, foobar :: Bool -> Int.
Then the type template for foobar is the following code.
-}

foobar :: Bool -> Int
foobar True  = undefined
foobar False = undefined

{-

Note how the type template elaborates the possible forms that Bool values
may take as patterns in the function definition.

The "undefined" on the right-hand sides above is important. It is a built-in
constant in Haskell.

Write out the type templates for functions of type:

prob1a :: Maybe a -> Int
prob1b :: [a] -> Int
prob1c :: Nat -> Int

Make sure to put your answers *outside* the comment and to make sure that they 
load correctly into ghci.
-}

data Tree   = Leaf Int | Node Int Tree Tree deriving Show

{- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
2. Build the following Tree with the constructors listed above:
                      0
                     / \
                    1   2
                   / \ / \
                   3 4 5 6

Fill in your answer below:
-} 

tree = undefined

{- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
3. The following function is defined is mathematical notation:

                            | x    if p is true
               cond p x y = |
                            | y    if p is false

   Define the function cond in Haskell below. Hint: define by cases based on p.
-}



{- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
4. Write a data declaration consisting of the names of six different countries. 
   Call this new data type Countries.
   Then, write a function that takes each country to its capital, written as 
   a string. Call this function "capital". Write your answers below outside 
   the comment. Be sure to write a type declaration for capital.
-}

              
{- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
5. Below is a function, stndrdth :: Int -> String, that takes an Int between 0 
   and 9 and returns its suffix (i.e., "st", "nd", "rd" or "th"). So, for 
   example, (stndrdth 3) returns "rd" because we write 3rd, (stndrdth 1)
   returns "st" because we write 1st, etc.

   (i) Simplify this function by replacing the clauses for 4 through 9 with a 
   single guard clause that checks whether i is between 4 and 9. Guards are 
   discussed in Chapter 4 of Learn You a Haskell for Great Good.
   (ii) Add a clause that checks whether i is less than 0 and returns 
        `error "no negatives!"` as a value (but get rid of the ` first).
        Warning: you must add this clause *before* the otherwise clause.
-}

stndrdth :: Int -> String
stndrdth i 
   | i==0      = "th"
   | i==1      = "st"
   | i==2      = "nd"
   | i==3      = "rd"
   | i==4      = "th"
   | i==5      = "th"
   | i==6      = "th"
   | i==7      = "th"
   | i==8      = "th"
   | i==9      = "th"
   | otherwise = stndrdth (i `rem` 10)            

{- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
6. Below is a data type declaration for a king and his subjects. Write an
instance declaration of Show for Person which behaves as:
*HW1> King
His Majesty the King
*HW1> Peer "Earl" "Lancaster" 9
The 9th Earl of Lancaster
*HW1> Knight "Reginald"
Sir Reginald
*HW1> Peasant "Harlan Scruggs"
Harlan Scruggs
*HW1> 
-}

data Person = King 
            | Peer String String Int
            | Knight String
            | Peasant String

{- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
7. What are the types of the constructors King, Peer, Knight and Peasant?
   Write your answers inside this comment.

-}

{- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
8. Below is an attempt to write a function taking Person's to Int, but
   it contains a number of errors in the way that the patterns are written.
   Correct these errors and move the corrected code outside of the comment
   to make sure that you've got it right.

level :: Person -> Int
level Kong           = 1
level (Peer (_,_,_)) = 2
level (Knightname)   = 3
level Peasant name   = 4

-}

{- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
9. Below is a data type declaration we have seen in class for simple
    arithmetic expressions.

    Define an Exp called e representing the following expression
        -(99 + -(101 + 72))
    Put your answer outside the comment.
-}

data Exp = Const Int | Neg Exp | Add Exp Exp


{-
10. Below is a skeleton of a function that finds a maximal number in
    a non-empty list of Ints. Here's what it does in English. If the list
    has only one number, then that number is maximal. If the list has at least
    two numbers, m and n, then it compares m and n.
    A. If m is greater it recursively calculates the maximal element in 
       the smaller list without n, or
    B. If m is not greater, then it recursively calculates the maximal item
       in the smaller list without m.

Convert the following skeleton into a working version of maxl by replacing
the undefine's.
-}

maxl :: [Int] -> Int
maxl [m]                     = undefined
maxl (m : n : ns) 
                 | m>n       = undefined
                 | otherwise = undefined
