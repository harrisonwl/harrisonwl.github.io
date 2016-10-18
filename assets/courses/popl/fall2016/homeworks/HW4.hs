module HW4 where

import Prelude hiding (Either,Left,Right)

{-
What to submit (via Blackboard): a single file, named
HW2_pawprint.hs, where pawprint is your MU username. The file should
contain definitions for every function listed below. Furthermore,
everyone should adhere to the following guidelines to get full credit:

* Your submission must successfully load and typecheck in Haskell Platform to
get any points. For example, executing:
     $ ghci HW4_pawprint.hs
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
question. This homework is due on Wednesday, October 26th, by 3pm.

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
Reverse Polish notation (RPN) is an alternative notation to the more commonly seen infix arithmetic
notation. Unlike infix notation, RPN and prefix notation do not require conventions regarding the
order of operations.  Instead, the order of evaluation is dictated by the syntax. With RPN,
operands are followed by their operators and evaluated accordingly.  In this assignment, we
will implement an RPN interpreter like one you might see in an HP calculator.  For more information on
RPN, Wikipedia has an excellent article (https://en.wikipedia.org/wiki/Reverse_Polish_notation).

Here is the declaration of the language you will interpret.
-}

data Op          = Val Int | Plus | Minus | Mul | IntDiv deriving (Show, Eq)
type PExp        = [Op]

{-
Our operators and operands are both represented by the Op type.
Whole arithmetic expressions in RPN are represented as lists of operations.
Evaluation for RPN works by reading a stream of inputs from front to back.
If a number (i.e., Val i) is read, it (i.e., i) is pushed on a stack. If any
operator is read, its operands are popped off of the stack, the operation is
performed with them and the result is pushed back onto the stack. The topmost
value on the stack becomes the rightmost argument to an operator. For example,
the input "2 5 -" should evaluate to -3.  Correct computations in RPN
result in an empty input and a stack with only one number value.  Stacks
with more than one value in them at the end of evaluation result from
malformed input.

Hint: to represent a stack of Int, you can simply use [Int].
-}

{-
Problem 1.

Write a function called, rpnParse, that parses a String and returns a PExp.  Input
strings for this function are tokens that are either numbers or operators separated
by whitespace.  Numbers can be any number of characters long.

Note: You do not need to validate that the input is a well-formed, RPN expression.

Hint: Converting String to other data types (like Int) can be done using the read
function in Haskell.  Review the documentation and examples of this function online
to see how it works. Here is an example of how rpnParse should work:

    ghci> rpnParse "+ - 200"
      [Plus,Minus,Val 200]
-}

rpnParse = undefined

{-
Problem 2.

Write an evaluator for RPN called eval.  This function should be typed as PExp -> Int.
Cases of bad input and evaluation should result in a call to the Haskell error function.

Hint: you may need to define a helper function that takes a stack and a PExp and
returns an Int. Use this helper function to define eval.
-}

eval :: PExp -> Int
eval = undefined

{-
Problem 3. Refactoring the evaluator.

The evaluator crashes in cases where there are bad inputs and division by zero.
This isn't particularly useful for recovery purposes.  We can refactor the evaluator
by using the data type, Either a b, to allow us to return a valid result or a failure
indicator.  Note the following code:
-}

data RPNError   = DivByZero | InvalidInput deriving (Show, Eq)
data Either a b = Left a | Right b deriving (Show, Eq)
type RPNResult  = Either RPNError Int

{-
By convention, the data type, Either a b, is either the failure case
(i.e., Left a) or the success case (i.e., Right b).  Implement a function called
evalSafe that is of type PExp -> RPNResult.

Hint: you may need to define a helper function that takes a stack and a PExp and
returns an RPNResult. Use this helper function to define evalSafe.

Note: This problem will not simply encapsulate your work from Problem 2, the error
function in Haskell (a hack of sorts) crashes the program and isn't catchable in
a pure function.  Some example outputs follow:

      ghci> evalSafe [Val 5, Val 0, IntDiv]
        Left DivByZero
      ghci> evalSafe [IntDiv, Plus, Val 0]
        Left InvalidInput
      ghci> evalSafe [Val 5, Val 1, Val 1, Plus, Mul]
        Right 10 
-}

evalSafe :: PExp -> RPNResult
evalSafe = undefined

{-
Problem 4. Translating to infix.

Lastly, implement a function rpnTrans that takes PExp and (given correct input) returns
a String of an equivalent arithmetic expression in infix notation, with the correct
corresponding order of operations enforced using parentheses.  This translation process
is still prone to some failure on bad inputs, so we should use a similar Either
configuration, but instead of creating a special type to represent it, we will return
a String in the failure case as well.  Formulating the correct type signature of rpnTrans
is left to the student.

    ghci> rpnTrans [Val 1, Val 1, Plus]
      Right "(1 + 1)"
    ghci> rpnTrans [Val 2, Val 4, Plus, Val 3, IntDiv]
      Right "((2 + 4) / 3)"
    ghci> rpnTrans [Val 2]
      Right "2"
    ghci> rpnTrans [Plus]
      Left "Bad Input."

-}

rpnTrans = undefined

{-
Problem 5. Full circle.

Using your previous work, implement a function, fullTrans, that translates an input
String in prefix notation to an output string in the same kind of Either formulation
you used in Problem 4.
-}

fullTrans = undefined
