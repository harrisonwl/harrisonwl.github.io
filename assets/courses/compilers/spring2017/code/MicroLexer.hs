module MicroLexer where

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
-- define which strings constitute tokens
--
recognize :: String -> Token
recognize str = case str of
  "begin" -> BEGIN
  "end"   -> END
  "read"  -> READ
  "write" -> WRITE
  "("     -> LPAREN
  ")"     -> RPAREN
  ";"     -> SEMICOLON
  ":="    -> ASSIGNOP
  "+"     -> PLUSOP
  _       -> ID str

--
-- a test case
--

test = "begin\n x:=7+y;\n read(y,z);\n end"

run (P x) inp = x inp

lexer :: Parser String
lexer = identifier
          +++ many1 digit
          +++ symbol ":="
          +++ symbol ";"
          +++ symbol "("
          +++ symbol ")"
          +++ symbol ","
          +++ symbol "+"

-- Recall Maybe type:
--   data Maybe a = Just a | Nothing

microlex :: String -> Maybe [Token]
microlex inp = case run (many lexer) inp of
                    [(toks,"")] -> Just $ map recognize toks ++ [SCANEOF]
                    _           -> Nothing

  
