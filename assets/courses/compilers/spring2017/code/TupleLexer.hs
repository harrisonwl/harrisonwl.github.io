module TupleLexer where

import Parsing

--
-- define what a token is
--
data Token = ASSIGN | WRITEI | READI | ADDI | SUBI |
             JUMP | JNZ | LABEL |
             SP | BP | FP | R String |
             LITERAL String | PLUS |
             M |
             LPAREN | RPAREN | LBRACKET | RBRACKET | COMMA |
             SCANEOF
           deriving Show


--
-- a test case
--
              
test = "(ASSIGN,BP,R99)\n (ASSIGN,SP,0)\n (JUMP,M[FP])\n (LABEL,3) "

run (P x) inp = x inp

number :: Parser Token
number = do
  n <- many1 digit
  return (LITERAL n)

assign :: Parser Token
assign = do
  symbol "ASSIGN"
  return ASSIGN

writei :: Parser Token
writei = do
  symbol "WRITEI"
  return WRITEI

readi :: Parser Token
readi = do
  symbol "READI"
  return READI

addi :: Parser Token
addi = do
  symbol "ADDI"
  return ADDI

subi :: Parser Token
subi = do
  symbol "SUBI"
  return SUBI

jump :: Parser Token
jump = do
  symbol "JUMP"
  return JUMP

jnz :: Parser Token
jnz = do
  symbol "JNZ"
  return JNZ

label :: Parser Token
label = do
  symbol "LABEL"
  return LABEL

opcodes = assign +++ writei +++ readi +++ addi +++ subi +++ jump
                 +++ jnz +++ label

memref :: Parser Token
memref = do
  symbol "M"
  return M

lparen :: Parser Token
lparen = do
  symbol "("
  return LPAREN

rparen :: Parser Token
rparen = do
  symbol ")"
  return RPAREN

lbracket :: Parser Token
lbracket = do
  symbol "["
  return LBRACKET

rbracket :: Parser Token
rbracket = do
  symbol "]"
  return RBRACKET

comma :: Parser Token
comma = do
  symbol ","
  return COMMA

plusop :: Parser Token
plusop = do
  symbol "+"
  return PLUS

fp :: Parser Token
fp = do
  symbol "FP"
  return FP

sp :: Parser Token
sp = do
  symbol "SP"
  return SP

bp :: Parser Token
bp = do
  symbol "BP"
  return BP

register :: Parser Token
register = do
  symbol "R"
  num <- many1 digit
  return (R num)

lexer :: Parser Token
lexer = number
         +++ opcodes
         +++ memref
         +++ lparen +++ rparen
         +++ lbracket +++ rbracket
         +++ comma
         +++ plusop
         +++ sp +++ bp +++ fp +++ register
         
tuplelex :: String -> Maybe [Token]
tuplelex inp = case run (many lexer) inp of
                    [(toks,"")] -> Just (toks ++ [SCANEOF])
                    _           -> Nothing

  
