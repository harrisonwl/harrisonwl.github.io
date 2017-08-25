module CS4430_HW1_Free where

import Parsing

--test = "(ASSIGN,BP,0)(ASSIGN,SP,0)(ASSIGN,FP,SP)(ASSIGN,M[FP],3)(ADDI,SP,FP,2)(JUMP,2)(LABEL,0)(ADDI,SP,SP,1)(ADDI,SP,SP,1)(ASSIGN,R0,M[FP+4])(READI,M[FP+4])(ASSIGN,R1,M[FP+5])(ASSIGN,R2,4)(ASSIGN,M[FP+5],R2)(ASSIGN,M[SP],4)(ADDI,SP,SP,1)(ASSIGN,M[SP],FP)(ADDI,SP,SP,1)(SUBI,M[SP],SP,2)(ADDI,SP,SP,1)(ASSIGN,R3,M[FP+4])(ASSIGN,R4,R3)(ASSIGN,M[SP],R4)(ADDI,SP,SP,1)(ASSIGN,R5,M[FP+5])(ASSIGN,R6,R5)(ASSIGN,M[SP],R6)(ADDI,SP,SP,1)(SUBI,FP,SP,5)(JUMP,1)(LABEL,4)(ASSIGN,SP,M[FP+2])(ASSIGN,FP,M[FP+1])(JUMP,M[FP])(LABEL,1)(ASSIGN,R7,M[FP+3])(ASSIGN,R8,R7)(ASSIGN,R9,M[FP+4])(ASSIGN,R10,R9)(ADDI,R11,R8,R10)(WRITEI,R11)(JUMP,M[FP])(LABEL,2)(ADDI,SP,SP,1)(ASSIGN,R12,M[FP+5])(READI,M[FP+5])(ASSIGN,R13,M[FP+5])(ASSIGN,R14,R13)(WRITEI,R14)(ASSIGN,R15,M[FP+6])(ASSIGN,R16,R15)(WRITEI,R16)(ASSIGN,M[SP],5)(ADDI,SP,SP,1)(ASSIGN,M[SP],FP)(ADDI,SP,SP,1)(SUBI,M[SP],SP,2)(ADDI,SP,SP,1)(ASSIGN,R17,M[FP+5])(ASSIGN,R18,R17)(ASSIGN,M[SP],R18)(ADDI,SP,SP,1)(SUBI,FP,SP,4)(JUMP,0)(LABEL,5)(ASSIGN,SP,M[FP+2])(ASSIGN,FP,M[FP+1])(JUMP,M[FP])(LABEL,3)"

--test1 = "(ASSIGN,BP,R99)\n (ASSIGN,SP,0)\n (JUMP,M[FP])\n (LABEL,3)"

data Token = ASSIGN | WRITEI | READI | ADDI
           | SUBI | JUMP | JNZ | LABEL | SP
           | BP | FP | R String | LITERAL String
           | M | LPAREN | RPAREN | COMMA | RBRACKET
           | LBRACKET | PLUS | SCANEOF
           deriving Show

recognize :: String -> Maybe Token
recognize str = case str of
  "ASSIGN" -> Just ASSIGN
  "WRITEI" -> Just WRITEI
  "READI"  -> Just READI
  "ADDI"   -> Just ADDI
  "SUBI"   -> Just SUBI
  "JUMP"   -> Just JUMP
  "JNZ"    -> Just JNZ
  "LABEL"  -> Just LABEL
  "SP"     -> Just SP
  "BP"     -> Just BP
  "FP"     -> Just FP
  "M"      -> Just M
  _        -> Nothing

noleadzerodigits :: Parser String
noleadzerodigits = do
  x  <- digit
  xs <- many digit
  if (x /= '0') || (xs == []) then return ([x] ++ xs) else failure

keyword :: Parser Token
keyword = do
  i <- token upperident
  case recognize i of
    Just tok -> return tok
    Nothing  -> failure
  where
    upperident :: Parser String
    upperident = do x  <- upper
                    xs <- many letter
                    return (x:xs)

literal :: Parser Token
literal = do
  space
  num <- noleadzerodigits
  space
  return (LITERAL num)

register :: Parser Token
register = do
  space
  x   <- char 'R'
  num <- noleadzerodigits
  space
  return (R num)

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

lexer :: Parser Token
lexer = keyword
        +++ literal
        +++ register
        +++ lparen
        +++ rparen
        +++ lbracket
        +++ rbracket
        +++ comma
        +++ plusop

run (P x) inp = x inp

tuplelex :: String -> Maybe [Token]
tuplelex inp = case run (many lexer) inp of
                    [(toks,"")] -> Just (toks ++ [SCANEOF])
                    _           -> Nothing
