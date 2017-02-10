module RollYourOwnLexer where

--
-- https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Char.html
--
import Data.Char

-- In particular, we're using:
--   isAlpha    :: Char -> Bool
--   isDigit    :: Char -> Bool
--   isSpace    :: Char -> Bool
--   digitToInt :: Char -> Int


-- typedef enum token_types { BEGIN, END, READ, WRITE,
--                            ID, INTLITERAL, LPAREN,RPAREN,
--                            SEMICOLON, COMMA,ASSIGNOP, PLUSOP,
--                            MINUSOP,SCANEOF
--                          } token;

--
-- define what a token is
--
data Token = BEGIN | END | READ | WRITE | ID String
           | INTLITERAL Int | LPAREN | RPAREN
           | SEMICOLON | COMMA | ASSIGNOP | PLUSOP
           | MINUSOP | SCANEOF
           deriving Show

--
-- define which strings constitute tokens
--

keyword :: String -> Token
keyword str = case str of
  "begin" -> BEGIN
  "end"   -> END
  "read"  -> READ
  "write" -> WRITE
  _       -> ID str

--
-- a test case
--

test = "begin\n x:=7+y;\n read(y,z);\n end"

-- Want something like this:
-- ghci> scanName "" "heypal ..."
-- (ID "heypal"," ...")
-- ghci> scanName "" "begin ..."
-- (BEGIN," ...")

scanName :: String -> String -> (Token,String)
scanName s [] = (keyword s,[])
scanName s (c:cs) | isAlpha c = scanName (s ++ [c]) cs
                  | otherwise = (keyword s,c:cs)

-- Want something like this:
--   ghci> scanInt 0 "1965heypal"
--   (1965,"heypal")
--   ghci> scanInt 0 "heypal"
--   (0,"heypal")

scanInt :: Int -> String -> (Int,String)
scanInt n []     = (n,[])
scanInt n (c:cs) | isDigit c = scanInt (n * 10 + digitToInt c) cs
                 | otherwise = (n,c:cs)

-- Want something like:
-- ghci> scan test
-- [BEGIN,ID "x",ASSIGNOP,INTLITERAL 7,PLUSOP,ID "y",SEMICOLON,READ,LPAREN,ID "y",COMMA,ID "z",RPAREN,SEMICOLON,END,SCANEOF]
--

scan :: String -> [Token]
scan str = case str of
  []                 -> [SCANEOF]
  (c:cs) | isAlpha c -> let
                           (tok,str') = scanName "c" cs
                        in
                           tok : scan str'
         | isSpace c -> scan cs
         | isDigit c -> let
                           (i,str') = scanInt 0 str
                        in
                           INTLITERAL i : scan str'
         | c==','    -> COMMA : scan cs
         | c=='('    -> LPAREN : scan cs
         | c==')'    -> RPAREN : scan cs
         | c=='-'    -> MINUSOP : scan cs
         | c=='+'    -> PLUSOP : scan cs
         | c==';'    -> SEMICOLON : scan cs
         | c==':'    -> case cs of
                             ('=':cs') -> ASSIGNOP : scan cs'
                             _         -> error "Lexical Error"
                        

  
