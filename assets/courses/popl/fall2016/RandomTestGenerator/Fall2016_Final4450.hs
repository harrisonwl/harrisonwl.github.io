module Final2016_Final4450 where

import System.IO
import System.Random
import Data.List
import Preamble
import Generator

  
--------------------
--- Example
--------------------

test = [tf1,m1,mc2,matching1,matching2,cfg,abstractsyntax]
l    = [blank,interp,interptext]
   where
     blank = Plain "\\begin{center}This page intentionally left blank. Exam continues on next page.\\end{center}"

tf1 = TF dir qs
  where
    dir = "Indicate which of the following statements are true or false by marking {\\bf True} or {\\bf False}."
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


m1 = Mult d1 [MultChoice dir qs, MultChoice dir2 qs2]
       where dir = "For which \\verb+q :: Prop+ does \\verb+(trans q)+ produce:"
                   ++ "\\begin{verbatim}\n"
                   ++ "(Vbl \"p\" :| Vbl \"p\") :| ((Vbl \"p\" :| Vbl \"p\") :| (Vbl \"p\" :| Vbl \"p\"))\n"
                   ++ "\\end{verbatim}\n"
             qs  = [
                    "\\verb+q+ is \\verb+(Imply (Not (Atom \"p\")) (Atom \"p\"))+\n",
                    "\\verb+q+ is \\verb+(Not (And (Atom \"p\") (Not (Atom \"p\"))))+\n",
                    "\\verb+q+ is \\verb+(Or (Atom \"p\") (Not (Atom \"p\")))+\n",
                    "\\verb+q+ is either \\verb+(Not (And (Atom \"p\") (Not (Atom \"p\"))))+ or \\verb+(Or (Atom \"p\") (Not (Atom \"p\")))+\n",
                    "None of these.\n"
                    ]
             d1 = "The following questions concern the translation \\verb+trans+ from {\\tt Prop} to {\\tt NAND}.\n\n"
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
               ++ "\n"
               ++ "\\end{verbatim}\n"
               ++ "}\n"
               ++ "\\end{minipage}\n\n"

             dir2 = "For which \\verb+q :: Prop+ does \\verb+(trans q)+ produce:" ++
                "{\\scriptsize\\begin{verbatim}\n" ++ 
                "(Vbl \"p\" :| ((Vbl \"p\" :| Vbl \"p\") :| (Vbl \"p\" :| Vbl \"p\"))) :| (Vbl \"p\" :| ((Vbl \"p\" :| Vbl \"p\") :| (Vbl \"p\" :| Vbl \"p\")))\n" ++ 
                "\\end{verbatim}}\n"
             qs2 = [
                "\\verb+q+ is \\verb+(Or (Atom \"p\") (And (Atom \"p\") (Atom \"p\")))+\n", 
                "\\verb+q+ is \\verb+(Imply (Atom \"p\") (Or (Atom \"p\") (Atom \"p\")))+\n",
                "\\verb+q+ is \\verb+(And (Atom \"p\") (And (Atom \"p\") (Atom \"p\")))+\n",
                "\\verb+q+ is \\verb+(Or (Atom \"p\") (Or (Atom \"p\") (Atom \"p\")))+\n",
                "None of these.\n"
               ]



mc2 = Mult dir (map (\ q -> ShortAnswer q 1) qs)
  where
    dir = "Here is the code for the {\\tt foldr} function and four functions defined in terms of it:"
       ++ "\\begin{verbatim}\n"
       ++ "foldr :: (a -> b -> b) -> b -> [a] -> b\n"
       ++ "foldr f z []     = z\n"
       ++ "foldr f z (x:xs) = x `f` foldr f z xs\n"
       ++ "\n"
       ++ "fee x y = foldr (:) y x\n"
       ++ "fie x   = foldr (\\ _ xs -> 1 + xs) 0 x\n"
       ++ "foe x   = foldr (+) 0 x\n"
       ++ "fum     = foldr (&&) True\n"
       ++ "   where (&&) :: Bool -> Bool -> Bool\n"
       ++ "         False && _   = False\n"
       ++ "         _ && False   = False\n"
       ++ "         True && True = True\n"
       ++ "\\end{verbatim}\n"
    qs = [
      "What is the type of \\verb|fee|? What does it do?\\vspace{10ex}",
      "What is the type of \\verb|fie|? What does it do?\\vspace{10ex}",
      "What is the type of \\verb|foe|? What does it do?\\vspace{10ex}",
      "What is the type of \\verb|fum|? What does it do?\\vspace{10ex}"
         ]

matching1 = Match dir qs ms
  where dir =
          "For each of the Haskell declarations  below on the left,"
          ++ "its type occurs on the right below. Select the matching type from the answer bank on the right."
          ++ "Each declaration has a matching type, but not every type in the answer bank has a declaration (i.e. "
          ++ "{\\bf you will not use every answer} in the answer bank). \n\n \\vspace{5ex}"
        qs = [
             "\\begin{verbatim} f (x, y) = (y, x) \\end{verbatim}\n",
             "\\begin{verbatim} f a = \"buzz\" \\end{verbatim}\n",
             "\\begin{verbatim} f g a b = g a b \\end{verbatim}\n",
             "\\begin{verbatim} f g h a = (g a, f a) \\end{verbatim}\n",
             "\\begin{verbatim} f x = show x \\end{verbatim}\n",
             "\\begin{verbatim} f g a b = g a == g b \\end{verbatim}\n",
             "\\begin{verbatim}f x = case x of \n           Nothing -> 0\n           Just n  -> n\\end{verbatim}\n",

             "\\begin{verbatim}f x = case x of\n       Nothing -> undefined\n       Just n  -> n\\end{verbatim}\n",
             "\\begin{verbatim}f = [(Just \"zed\",Nothing),\n     (Just \"fox\",Just 1)]\\end{verbatim}\n",
             "\\begin{verbatim}f = [(Nothing,Nothing)]\\end{verbatim}\n"]

        ms = [
          "\\begin{verbatim}:: (z,y) -> (y,z)\\end{verbatim}\n",
          "\\begin{verbatim}:: [(Maybe a, Maybe a)]\\end{verbatim}\n",
          "\\begin{verbatim}:: (y -> z -> a) -> y -> z -> a\\end{verbatim}\n",
          "\\begin{verbatim}:: Show a => a -> String\\end{verbatim}\n",
          "\\begin{verbatim}:: (a -> Bool) -> a -> a -> Bool\\end{verbatim}",
          "\\begin{verbatim}:: a -> String\\end{verbatim}\n",
          "\\begin{verbatim}:: Num a => Maybe a -> a\\end{verbatim}\n",
          "\\begin{verbatim}:: Maybe zed -> zed\\end{verbatim}\n",
          "\\begin{verbatim}:: (Bool -> Bool -> Bool) -> \n      Bool -> Bool -> Bool\\end{verbatim}\n",
          "\\begin{verbatim}:: (a -> b) -> (a -> b)\n       -> a -> (b,b)\\end{verbatim}\n",
          "\\begin{verbatim}:: Num a => [(Maybe String,Maybe a)]\\end{verbatim}\n",
          "\\begin{verbatim}:: String\\end{verbatim}\n",
          "\\begin{verbatim}:: Show a => (Maybe a, Maybe a)\\end{verbatim}\n",
          "\\begin{verbatim}:: Ord b  => [(Maybe b, Maybe b)]\\end{verbatim}\n"
          ]


matching2 = Match dir qs ms
  where
    dir = "For each of the Haskell declarations of \\verb+e+ below on the left, its type occurs on the right below. Draw a line from the declaration to its type on the right.\n" ++
          "\\begin{verbatim}map     :: (a -> b) -> [a] -> [b]\n" ++
          "id      :: a -> a\n" ++
          "foldr   :: (a -> b -> b) -> b -> [a] -> b\n" ++
          "(.)     :: (a -> b) -> (c -> a) -> c -> b\n" ++
          "reverse :: [a] -> [a]\n" ++
          "curry   :: ((a,b) -> c) -> (a -> b -> c)\n" ++
          "uncurry :: (a -> b -> c) -> (a,b) -> c\n" ++
          "concat  :: [[a]] -> [a]\n" ++
          "take    :: Int -> [a] -> [a]\n" ++
          "\\end{verbatim}\n"
    qs = ["\\verb+e = map reverse+\\vspace{3ex}\n",
          "\\verb+e = map (.)+\\vspace{3ex}\n",
          "\\verb+e = (.) map+\\vspace{3ex}\n",
          "\\verb+e = map . foldr+\\vspace{3ex}\n",
--          "\\verb+e = foldr id+\\vspace{3ex}\n",
          "\\verb+e = foldr . id+\\vspace{3ex}\n",
--          "\\verb+e = map . take+\\vspace{3ex}\n",
          "\\verb+e = curry . uncurry+\\vspace{3ex}\n",
          "\\verb+e = uncurry . curry+\\vspace{3ex}\n",
          "\\verb+e = map concat+\\vspace{3ex}\n"
          ]
    ms = [
           "\\verb+:: (a -> b -> b) -> b -> [a] -> b+\\vspace{3ex}\n",
           "\\verb+:: (a -> b -> b) -> [b] -> [[a] -> b]+\\vspace{3ex}\n",
           "\\verb+:: a -> [a -> a] -> a+\\vspace{3ex}\n",
           "\\verb+:: (a -> b -> c) -> a -> b -> c+\\vspace{3ex}\n",
           "\\verb+:: [[[a]]] -> [[a]]+\\vspace{3ex}\n",
           "\\verb+:: Int -> [[a]] -> [[a]]+\\vspace{3ex}\n",
           "\\verb+:: [[a]] -> [[a]]+\\vspace{3ex}\n",
           "\\verb+:: [a -> b] -> [(c -> a) -> c -> b]+\\vspace{3ex}\n",
           "\\verb+:: (a -> b -> c) -> a -> [b] -> [c]+\\vspace{3ex}\n",
           "\\verb+:: ((a,b) -> c) -> (a,b) -> c+\\vspace{3ex}\n"
         ]
      
cfg = Mult dir [q1,q2]
   where
     dir = "Consider the following context-free grammar for \\<Exp\\>:" ++
           "\\begin{haskell}\n" ++
           "Exp &::=&\\relax Identifer | ( {\\mathbf{lambda}} Identifier Exp ) | ( Exp Exp )\n" ++
           "\\end{haskell}\n" ++
           "An \\<Identifier\\> can be any non-empty string from \\<\\{\\mathbf{a},\\ldots,\\mathbf{z}\\}\\>.\n" ++
           "\n" 
     q1 = MultChoice dir1 qs1
        where
          dir1 = "Which of the following are {\\bf NOT} in the language of \\<Exp\\>?\n"
          qs1  = [
           "\\<( {\\mathbf{lambda}} )\\>" ,
           "\\<( {\\mathbf{lambda}} {\\mathbf{lambda}} )\\>" ,
           "\\<( {\\mathbf{lambda}} {\\mathbf{lambda}} {\\mathbf{lambda}} )\\>",
           "\\<( {\\mathbf{lambda}} {\\mathbf{lambda}} ({\\mathbf{lambda}} {\\mathbf{lambda}}))\\>" ,
           "None of these."
           ]
     q2 = ShortAnswer "Below, write a Haskell data type declaration which represents \\<Exp\\>." 10

abstractsyntax = Mult dir [q1,q2]
  where
    dir = "Below, you will find a BNF grammar:\n" ++
          "\\begin{haskell}\n" ++
          "Plant &::=&\\relax leaf | (list Plant stem Plant) | (list Plant)\n" ++
          "\\end{haskell}\n"
    q1 = MultChoice dir1 qs1
      where
         dir1 = "Circle all those below that are in the language of \\<Plant\\>.\n"
         qs1 = [
           "\\<leaf\\>\n",
           "\\<(list stem)\\>\n",
           "\\<(list (list leaf stem leaf) stem (list leaf))\\>\n",
           "\\<(list (list (list leaf)))\\>\n",
           "All of these.\n"
           ]
    q2 = MultChoice dir2 qs2
      where
        dir2 = "Circle the data declaration which best represents \\<Plant\\>.\n"
        qs2  = [
              "\\verb+data Plant = Leaf | Two [(Plant,String,Plant)] | One [Plant]+\n",
              "\\verb+data Plant = LeafStem String | Two [(Plant,Plant,Plant)] | One [Plant]+\n",
              "\\verb+data Plant = Leaf | Stem | Two [(Plant,Stem,Plant)] | One [Plant]+\n",
              "\\verb+data Plant = Leaf | Two (Plant,Plant) | One Plant+\n",
              "\\verb+data Plant = Leaf | List | Stem | Four Plant Plant Plant Plant | Two Plant Plant+\n"
              ]

interp = Mult dir [q]
   where
     dir = 
         "Say we introduce a mysterious new kind of binding to the  interpreters called \\verb+myst+.  ``Mystery'' binding has the same syntactic form as other forms we have seen this semester; here is an example:\n" ++
         "\\begin{verbatim}\n" ++
         "        myst s 5; myst x s; myst s 10; x\n" ++
         "\\end{verbatim}\n" ++
         "Assume that the abstract syntax for \\verb+myst+ is defined by this new clause in the \\verb+Exp+ data type: \n" ++
         "\\begin{verbatim}\n" ++
         "       data Exp = ... | Mystery String Exp Exp\n" ++
         "\\end{verbatim}\n" ++
         "The expression above is parsed into the \\verb+Exp+ abstract syntax as:\n" ++
         "\\begin{verbatim}\n" ++
         "       Declare \"s\" (Literal (IntV 5)) \n" ++
         "           (Mystery \"x\" (Variable \"s\") \n" ++
         "                (Declare \"s\" (Literal (IntV 10)) \n" ++
         "                     (Variable \"x\")))\n" ++
         "\\end{verbatim}\n"

     q = MultChoice dir1 qs1
        where
          dir1 = "{\\bf (4 points)} Consulting the interpreter code on the next page, calculate the value of the above expression by hand.  The value produced by the above \\verb+Term+ is:\n"
          qs1 = [
              "\\verb+(IntV 5)+\n",
              "\\verb+(IntV 10)+\n",
              "\\verb+unbound variable: s+\n",
              "\\verb+unbound variable: x+\n"
              ]

interptext = Plain code
   where
     code = 
         "{\\footnotesize\n" ++
         "\\begin{verbatim}\n" ++
         "data Value = IntV  Int\n" ++
         "           | BoolV Bool\n" ++
         "           | ClosureV String Exp Env\n" ++
         "           | MystV Exp \n" ++
         "  deriving (Eq, Show)\n" ++
         "\n" ++
         "data Exp = Literal   Value\n" ++
         "         | Unary     UnaryOp Exp\n" ++
         "         | Binary    BinaryOp Exp Exp\n" ++
         "         | If        Exp Exp Exp\n" ++
         "         | Variable  String\n" ++
         "         | Declare   String Exp Exp\n" ++
         "         | Mystery   String Exp Exp\n" ++
         "         | Function  String Exp     \n" ++
         "         | Call      Exp Exp        \n" ++
         "  deriving (Eq, Show)\n" ++
         "  \n" ++
         "type Env = [(String, Value)]\n" ++
         "\n" ++
         "evaluate :: Exp -> Env -> Value\n" ++
         "evaluate (Literal v) env = v\n" ++
         "\n" ++
         "evaluate (Unary op a) env = \n" ++
         "  unary op (evaluate a env)\n" ++
         "\n" ++
         "evaluate (Binary op a b) env = \n" ++
         "  binary op (evaluate a env) (evaluate b env)\n" ++
         "\n" ++
         "evaluate (If a b c) env = \n" ++
         "  let BoolV test = evaluate a env in\n" ++
         "    if test then evaluate b env\n" ++
         "            else evaluate c env\n" ++
         "\n" ++
         "evaluate (Variable x) env = fromJust x (lookup x env)\n" ++
         "   where\n" ++
         "     fromJust x (Just (MystV e)) = evaluate e env\n" ++
         "     fromJust x (Just v) = v\n" ++
         "     fromJust x Nothing  = error (\"unbound variable: \" ++ x)\n" ++
         "\n" ++
         "evaluate (Function x body) env = ClosureV x body env\n" ++
         "\n" ++
         "evaluate (Declare x exp body) env = evaluate body newEnv\n" ++
         "  where newEnv = (x, evaluate exp env) : env\n" ++
         "\n" ++
         "evaluate (Mystery x exp body) env = evaluate body newEnv\n" ++
         "  where newEnv = (x, MystV exp) : env\n" ++
         "\n" ++
         "evaluate (Call fun arg) env = evaluate body newEnv\n" ++
         "  where ClosureV x body closeEnv = evaluate fun env\n" ++
         "        newEnv = (x, evaluate arg env) : closeEnv\n" ++
         "\n" ++
         "execute exp = evaluate exp []\n" ++
         "\\end{verbatim}\n" ++
         "}\n" 
