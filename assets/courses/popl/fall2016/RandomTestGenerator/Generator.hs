module Generator where

import System.IO
import System.Random
import Data.List
import Preamble

demo = do
  g <- newStdGen
  print $ take 10 (randomRs ('a', 'z') g)  

demo' = do
  g <- newStdGen
  print $ take 10 (randomRs (x, y) g)  
    where x :: Float
          x = 0.0
          y :: Float
          y = 1.0


shuffle :: [a] -> IO [a]
shuffle l = do
  g <- newStdGen
  let nl = take ll $ randomRs (x,y) g
  let shuffled_l = map snd $ sortBy ordering $ zip nl l
  return $ shuffled_l
    where
      ll = length l
      x :: Float
      x = 0.0
      y :: Float
      y = 1.0
      ordering (x,_) (y,_) | x<y  = LT
                           | x==y = EQ
                           | x>y  = GT

shuffleTest :: Test -> IO Test
shuffleTest [] = return []
shuffleTest (q:qs) = do
  case q of
    TrueFalse dir questions -> do
      sq  <- shuffle questions
      qs' <- shuffleTest qs
      return $ TrueFalse dir sq : qs'
    Multiple dir questions  -> do
      sq  <- shuffle questions
      qs' <- shuffleTest qs
      return $ Multiple dir sq : qs' 
    Directions d -> do
      qs' <- shuffleTest qs
      return $ Directions d : qs'

generate :: String -> Test -> IO ()
generate file test = do
  hdl <- openFile ("TestDir/" ++ file) WriteMode
  hPutStrLn hdl frontmatter
  test' <- shuffleTest test
  hPutStrLn hdl $ foldr (\ q qs -> show q ++ qs) "" test'
  hPutStrLn hdl endmatter
  hClose hdl

generateTests tname test 0 = return ()
generateTests tname test n = do
  generate (tname ++ "_" ++ show n ++ ".tex") test
  generateTests tname test (n-1)

type Test     = [Question]    
data Question = TrueFalse String [String]
              | Multiple String [String]
              | Directions String


instance Show Question where
  show (TrueFalse dir qs) =
      "\\item [] {\\bf Directions.} " ++ dir ++ "\n" ++
--      "\\begin{enumerate}\n" ++
      (foldr (++) "" $ map (\ q -> "\\item {\\bf True} or {\\bf False}: " ++ q) qs) -- ++
--      "\\end{enumerate}"
  show (Multiple dir qs) =
      "\\item [] " ++ dir ++ "\n" ++
      "\\begin{enumerate}[(a)]\n" ++
      (foldr (++) "" $ map (\ q -> "\\item " ++ q) qs) ++
      "\\end{enumerate}"
  show (Directions d) = "\\item [] {\\bf Directions.} " ++ d ++ "\n"
  
--------------------
--- Example
--------------------

test = [tf1,d1,m1]

tf1 = TrueFalse dir qs
  where
    dir = "Indicate which of the following statements are true or false by marking {\\bf True} or {\\bf False} in the corresponding answer location on your answer sheet."
    qs = [
           "Haskell has higher order functions.",
           "Haskell is statically typed.",
           "Haskell terms are dynamically typed.",
           "Static typing increases execution time.",
           "In Haskell, {\\tt ((f x) y) == (f x y).}",
           "The {\\tt length} function can be defined as {\\tt length = map f} for some f.",
           "The {\\tt Maybe} type expresses computations that can fail.",
           "Correctly typed Haskell programs never crash.",
           "Haskell functions can be declared with different numbers of arguments.",
           "Side effects in programs are things that are unintended by programmers.",
           "The {\\tt type} reserved word in Haskell produces a new type.",
           "Data constructors in Haskell are actually functions.",
           "Global variables can be declared in Haskell using a let binding.",
           "Haskell Strings are a kind of array, like in C.",
           "Free variables are variables bound in some larger lexical scope.",
           "The {\\tt map} function is a kind of fold.",
           "Partial application is always performed by giving a function another function as an argument.",
           "Haskell data types support inheritance.",
           "(+ 2) has the type {\\tt :: Num a => a}.",
           "Haskell lists can be of infinite length."
         ]


d1 = Directions $
     "The following questions concern the translation from {\\tt Prop} to {\\tt NAND}."
     ++ "\\begin{minipage}[t]{2.7in}\n"
     ++ "{\\footnotesize\n"
     ++ "\\begin{verbatim}\n"
     ++ "data Prop = Atom String\n"
     ++ "          | Not Prop\n"
     ++ "          | Imply Prop Prop\n"
     ++ "          | Or Prop Prop\n"
     ++ "          | And Prop Prop\n"
     ++ "          | Iff Prop Prop\n"
     ++ "          deriving Show\n"
     ++ "\n"
     ++ "data NAND = Vbl String \n"
     ++ "          | NAND :| NAND \n"
     ++ "             deriving Show\n"
     ++ "\\end{verbatim}\n"
     ++ "}\n"
     ++ "\\end{minipage}\n"
     ++ "\\begin{minipage}[t]{2in}\n"
     ++ "{\\footnotesize\n"
     ++ "\\begin{verbatim}\n"
     ++ "trans :: Prop -> NAND\n"
     ++ "trans (Atom v)      = Vbl v\n"
     ++ "trans (Not p)       = trans p :| trans p\n"
     ++ "trans (Or p1 p2)    = (p1' :| p1') :| (p2' :| p2')\n"
     ++ "  where p1' = trans p1\n"
     ++ "        p2' = trans p2\n"
     ++ "trans (And p1 p2)   = (p1' :| p2') :| (p1' :| p2')\n"
     ++ "  where p1' = trans p1\n"
     ++ "        p2' = trans p2\n"
     ++ "trans (Imply p1 p2) = p1' :| (p2' :| p2')\n"
     ++ "  where p1' = trans p1\n"
     ++ "        p2' = trans p2\n"
     ++ "trans (Iff p1 p2)   = trans (And (Imply p1 p2) (Imply p2 p1))\n"
     ++ "\\end{verbatim}\n"
     ++ "}\n"
     ++ "\\end{minipage}\n"



m1 = Multiple dir qs
       where dir = "For which \\verb+q :: Prop+ does \\verb+(trans q)+ produce:"
                   ++ "\\begin{verbatim}\n"
                   ++ "(Vbl \"p\" :| Vbl \"p\") :| ((Vbl \"p\" :| Vbl \"p\") :| (Vbl \"p\" :| Vbl \"p\"))\n"
                   ++ "\\end{verbatim}\n"
             qs  = [
                    "\\verb+q+ is \\verb+(Imply (Not (Atom \"p\")) (Atom \"p\"))+\n",
                    "\\verb+q+ is \\verb+(Not (And (Atom \"p\") (Not (Atom \"p\"))))+\n",
                    "\\verb+q+ is \\verb+(Or (Atom \"p\") (Not (Atom \"p\")))+\n",
                    "(b) and (c)\n",
                    "None of the above.\n"
                    ]

