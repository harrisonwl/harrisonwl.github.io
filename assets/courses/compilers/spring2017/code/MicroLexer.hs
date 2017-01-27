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

scanName = identifier

deP (P x) = x
