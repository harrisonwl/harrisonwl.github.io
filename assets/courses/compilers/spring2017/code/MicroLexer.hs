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
           | INTLITERAL String | LPAREN | RPAREN
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
  _       -> ID str

--
-- a test case
--

test = "begin\n x:=7+y;\n read(y,z);\n end"

run (P x) inp = x inp

identORkeyword :: Parser Token
identORkeyword = do
  i <- identifier
  return (recognize i)

number :: Parser Token
number = do
  n <- many1 digit
  return (INTLITERAL n)

assign :: Parser Token
assign = do
  symbol ":="
  return ASSIGNOP

semicolon :: Parser Token
semicolon = do
  symbol ";"
  return SEMICOLON

lparen :: Parser Token
lparen = do
  symbol "("
  return LPAREN

rparen :: Parser Token
rparen = do
  symbol ")"
  return RPAREN

comma :: Parser Token
comma = do
  symbol ","
  return COMMA

plusop :: Parser Token
plusop = do
  symbol "+"
  return PLUSOP

lexer :: Parser Token
lexer = identORkeyword
          +++ number
          +++ assign
          +++ semicolon
          +++ lparen
          +++ rparen
          +++ comma
          +++ plusop

-- Recall Maybe type:
--   data Maybe a = Just a | Nothing

microlex :: String -> Maybe [Token]
microlex inp = case run (many lexer) inp of
                    [(toks,"")] -> Just (toks ++ [SCANEOF])
                    _           -> Nothing

  
