module HW3 where


{-
What to submit (via Blackboard): a single file, named
HW2_pawprint.hs, where pawprint is your MU username. The file should
contain definitions for every function listed below. Furthermore,
everyone should adhere to the following guidelines to get full credit:

* Your submission must successfully load and typecheck in Haskell Platform to
get any points. For example, executing:
     $ ghci HW3_pawprint.hs
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
question. This homework is due on Friday, October 7th, by 3pm.

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
The function foo below takes two functions, f and p, and a list xs and computes a
list using a list comprehension. Rewrite foo as prob1 using the functions map and filter instead of
list comprehensions.

Hint 1. First, figure out what foo does by applying it to some functions and lists.
Hint 2. Use ghci to determine the type of foo. The type of prob1 must be the same.
-}

foo f p xs = [ f x | x <- xs, p x ]

prob1 = undefined

{-
Problem 2.
Define a function, prob2, that takes a list of Int's and returns an Int. Function prob2 should
convert a list of digits along these lines:

ghci> prob2 [2,3,4,5]
2345

To receive credit for this problem, you MUST write your answer using foldl.

Hint. One approach that might be helpful is to write the answer first in accumulator passing
style and then convert that definition to one using foldl as we did a number of times in class.
-}

prob2 = undefined

{- Problem 3.
The following function is the familiar factorial function:
-}

fac :: Integer -> Integer
fac n | n <= 0    = 1
      | otherwise = n * fac (n-1)

prob3 :: Integer -> Integer -> Integer
prob3 memo n = undefined

{-
This function can be written using what's called "memoization", which has a very
similar feel to accumulator-passing style. Here is what prob3 should do. When n <= 0, it
should return memo. When n > 0, prob3 should call itself recursively with arguments
n times memo and n minus 1. Notice that prob3 is using the n argument as a counter and
accumulating the factorial value in the memo argument.
-}

{-
Problem 4. 
Recall that the Fibonacci sequence is: 1, 1, 2, 3, 5, 8, 21, 34, 55,...
The rule describing this sequence is that the nth item is equal to the (n-1)th item + the (n-2)th item.

If you'd like to read up about Fibonacci and his sequence, check out Wikipedia:
   https://en.wikipedia.org/wiki/Fibonacci_number

The Fibonacci sequence has a very straightforward implementation in Haskell:
-}

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

{-
The problem with this function is that it has complexity O(2 ^ n). This means that it has
atrocious run-time performance:
ghci> fib 10
89
ghci> fib 20
10946
ghci> fib 30
1346269
ghci> fib 40
    <...see you later...>

One way around this is to use memoization. So, n1 below should be the (n-1)th Fibonacci number and n2
should be the (n-2)th Fibonacci number. And, like the case with prob3, the n argument is used as
a counter.
-}

prob4 :: (Integer,Integer) -> Integer -> Integer
prob4 (n1,n2) n = undefined

{-
Your solution should behave like the transcript below. Part of the challenge of this problem is to
determine what the initial values of n1 and n2 should be. Here, I've written them below as ?1 and ?2,
which should be constants of type Integer.

ghci> prob4 (?1,?2) 0
1
ghci> prob4 (?1,?2) 1
1
ghci> prob4 (?1,?2) 2
2
ghci> prob4 (?1,?2) 3
3
ghci> prob4 (?1,?2) 4
5
ghci> prob4 (?1,?2) 5
8
ghci> prob4 (?1,?2) 6
13
ghci> prob4 (?1,?2) 7
21
   ...
-}

                    
{-
Problem 5.
Define the following function. It accepts three arguments, two functions from Int to Int (call them
f and g) and an Int (call it x). This function returns a result such that if x is even, then it returns
f(x), and g(x) otherwise. For example,

ghci> prob5 fac fib 4
24
ghci> prob5 fac fib 5
5
-}

prob5 = undefined
