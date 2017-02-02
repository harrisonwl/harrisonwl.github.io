module MicroLexer where

--
-- https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Char.html
--
import Data.Char
import Parsing

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
-- a test case
--

test = "begin\n x:=7+y;\n read(y,z);\n end"

deP (P x) = x
run (P x) inp = x inp

--microlex :: String -> Maybe [Token]
microlex inp = case run (many lexer) inp of
--                    [(toks,"")] -> Just (map recognize toks ++ [Just SCANEOF])
                    [(toks,"")] -> mapM recognize toks -- ++ [Just SCANEOF])  
                    _           -> Nothing

lexer :: Parser String
lexer = identifier
          +++ many1 digit
          +++ symbol ":="
          +++ symbol ";"
          +++ symbol "("
          +++ symbol ")"
          +++ symbol ","
          +++ symbol "+"

--
-- define which strings constitute tokens
--

-- Recall Maybe type:
--   data Maybe a = Just a | Nothing

recognize str = case str of
  "begin" -> Just BEGIN
  "end"   -> Just END
  "read"  -> Just READ
  "write" -> Just WRITE
  "("     -> Just LPAREN
  ")"     -> Just RPAREN
  ";"     -> Just SEMICOLON
  ":="    -> Just ASSIGNOP
  "+"     -> Just PLUSOP
