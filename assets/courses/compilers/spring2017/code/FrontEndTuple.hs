module FrontEndTuple where

import System.IO
import Data.Char

instance Show Register where
    show (Reg i) = "R" ++ (show i)
    show SP      = "SP"
    show BP      = "BP"
    show FP      = "FP"

instance Show RegArg where
    show (RgArg r)       = show r
    show (RegInd r)      = "M[" ++ show r ++ "]"
    show (RegIndOff r o) = "M[" ++ show r ++ "+" ++ show o ++ "]"

instance Show Tuple where
    show (Asn ra a)     = "(ASSIGN,"++show ra++","++show a++")"
    show (Add ra a1 a2) = "(ADDI,"++show ra++","++show a1++","++show a2++")"
    show (Sub ra a1 a2) = "(SUBI,"++show ra++","++show a1++","++show a2++")"
    show (Jmp a)        = "(JUMP,"++show a++")"
    show (JmpNZ r a)    = "(JNZ,"++show r++","++show a++")"
    show (Label i)      = "(LABEL,"++show i++")"
    show (Read ra)      = "(READI,"++show ra++")"
    show (Write a)      = "(WRITEI,"++show a++")"
    show Exit           = "EXIT"

instance Show Arg where
    show (RA ra)     = show ra
    show (Literal i) = show i

data Tuple    = Asn RegArg Arg
              | Add RegArg Arg Arg
              | Sub RegArg Arg Arg
              | Jmp Arg
              | JmpNZ RegArg Arg
              | Label Int
              | Read RegArg
              | Write Arg
              | Exit
data Register = Reg Int | SP | FP | BP deriving Eq
data RegArg   = RgArg Register | RegInd Register | RegIndOff Register Int
data Arg      = RA RegArg | Literal Int
type TupleProgram = [Tuple]

getit h = hIsEOF h >>= \ eof ->
          if eof then
              return ""
          else
              hGetLine h >>= \ l ->
              getit h >>= \ rest ->
              return (l ++ rest)

data Token = LPAR | RPAR | COMMA | ASSIGN
           | ADDI | SUBI | JUMP | JUMPNZ | LIT Int
           | READI | WRITEI | EXIT | REG Int | LABEL
           | FPtok | SPtok | BPtok
           | LBRACK | RBRACK | PLUS | M
           deriving (Show,Eq)

lexNum cs = LIT (read num) : lexer rest
    where (num,rest) = span isDigit cs

lexAlpha cs = let (as,rest) = span isAlpha cs in
 case as of
      "ADDI"   -> ADDI : lexer rest
      "SUBI"   -> SUBI : lexer rest
      "READI"  -> READI : lexer rest
      "WRITEI" -> WRITEI : lexer rest
      "ASSIGN" -> ASSIGN : lexer rest
      "LABEL"  -> LABEL : lexer rest
      "M"      -> M : lexer rest
      "FP"     -> FPtok : lexer rest
      "SP"     -> SPtok : lexer rest
      "BP"     -> BPtok : lexer rest
      "JUMP"   -> JUMP : lexer rest
      "JNZ"    -> JUMPNZ : lexer rest
      "EXIT"   -> EXIT : lexer rest
      "R"      -> let (LIT i :rs) = lexNum rest in REG i : rs

lexer :: String -> [Token]
lexer [] = []
lexer ('(':cs) = LPAR : lexer cs
lexer (')':cs) = RPAR : lexer cs
lexer ('[':cs) = LBRACK : lexer cs
lexer (']':cs) = RBRACK : lexer cs
lexer ('+':cs) = PLUS : lexer cs
lexer (',':cs) = COMMA : lexer cs
lexer ('#':cs) = consumeLine cs
lexer (c:cs)
     | isSpace c = lexer cs
     | isAlpha c = lexAlpha (c:cs)
     | isDigit c = lexNum (c:cs)

consumeLine []       = []
consumeLine ('#':cs) = lexer cs
consumeLine (c:cs)   = consumeLine cs

t2 = "(READI,SP) \n (WRITEI,SP)\n (READI,SP)\n (WRITEI,SP)\n (READI,SP)\n (WRITEI,SP)"

t0 = "(ASSIGN,M[SP+3],R11)"
t1 = "(ASSIGN,R11,R11)"

parseTuples []         = []
parseTuples (LPAR:ts)  = t : (parseTuples rest)
     where (t,rest) = parseTuple ts

-- RegArg   -> Register | M[Register] | M[Register + Num]
parseRegArg (REG i:ts) = (RgArg (Reg i), ts)
parseRegArg (SPtok:ts) = (RgArg SP, ts)
parseRegArg (FPtok:ts) = (RgArg FP , ts)
parseRegArg (BPtok:ts) = (RgArg BP, ts)
parseRegArg (M:LBRACK:r:RBRACK:ts) = (ra,ts)
      where ra = RegInd (register r)
parseRegArg (M:LBRACK:r:PLUS:(LIT o):RBRACK:ts) = (ra,ts)
      where ra = RegIndOff (register r) o
parseRegArg ts = error $ "parsing error around: " ++ (show $ twenty ts)

parseArg (REG i:ts)    = (RA $ RgArg (Reg i), ts)
parseArg (SPtok:ts)    = (RA $ RgArg SP, ts)
parseArg (FPtok:ts)    = (RA $ RgArg FP , ts)
parseArg (BPtok:ts)    = (RA $ RgArg BP, ts)
parseArg (M:LBRACK:r:RBRACK:ts) = (RA ra,ts)
      where ra = RegInd (register r)
parseArg (M:LBRACK:r:PLUS:(LIT o):RBRACK:ts) = (RA ra,ts)
      where ra = RegIndOff (register r) o
parseArg (LIT i:ts)    = (Literal i,ts)
parseArg ts = error $ "parsing error around: " ++ (show $ twenty ts)

register r = case r of
  (REG i) -> (Reg i)
  SPtok   -> SP
  FPtok   -> FP
  BPtok   -> BP
  _       -> error $ "not a register: " ++ (show r)

parseTuple (ASSIGN:ts) = let ts0       = comma ts
                             (ra,ts1)  = parseRegArg ts0
                             ts2       = comma ts1
                             (arg,ts3) = parseArg ts2
                         in
                             (Asn ra arg, rpar ts3)

parseTuple (ADDI:ts)   = let ts0       = comma ts
                             (ra,ts1)  = parseRegArg ts0
                             ts2       = comma ts1
                             (arg1,ts3) = parseArg ts2
                             ts4       = comma ts3
                             (arg2,ts5) = parseArg ts4
                         in
                             (Add ra arg1 arg2, rpar ts5)
parseTuple (SUBI:ts)   = let ts0       = comma ts
                             (ra,ts1)  = parseRegArg ts0
                             ts2       = comma ts1
                             (arg1,ts3) = parseArg ts2
                             ts4       = comma ts3
                             (arg2,ts5) = parseArg ts4
                         in
                             (Sub ra arg1 arg2, rpar ts5)
parseTuple (JUMP:ts)   = let ts0       = comma ts
                             (arg,ts1) = parseArg ts0
                         in
                             (Jmp arg, rpar ts1)
parseTuple (JUMPNZ:ts) = let ts0       = comma ts
                             (reg,ts1) = parseRegArg ts0
                             ts2       = comma ts1
                             (arg,ts3) = parseArg ts2
                         in
                             (JmpNZ reg arg, rpar ts3)
parseTuple (LABEL:ts)  = let ts0       = comma ts
                             (Literal i,ts1) = parseArg ts0
                         in
                             (Label i, rpar ts1)
parseTuple (READI:ts)  = let ts0       = comma ts
                             (arg,ts1) = parseRegArg ts0
                         in
                             (Read arg, rpar ts1)
parseTuple (WRITEI:ts) = let ts0       = comma ts
                             (arg,ts1) = parseArg ts0
                         in
                             (Write arg, rpar ts1)
parseTuple (EXIT:ts)   = (Exit, rpar ts)

constant c (t:ts) = if c==t then ts
                        else error $ "parsing error around: " ++ (show $ twenty ts)

comma = constant COMMA
rpar  = constant RPAR

first n [] = []
first 0 xs = []
first n (x:xs) = x : (first (n-1) xs)

twenty = first 20


{-
Tuple	 -> Asn | Add | Sub | Jmp | Label | Read | Write | Exit
Asn	 -> (ASSIGN,RegArg,Arg)
Add	 -> (ADDI,RegArg,Arg,Arg)
Sub	 -> (SUBI,RegArg,Arg,Arg)
Jmp	 -> (JUMP,Arg)
Label	 -> (LABEL,Num) /* N.b., labels are Nums */
Read	 -> (READI,RegArg)
Write	 -> (WRITEI,Arg)
Exit	 -> (EXIT)
Arg	 -> RegArg | Num
Register -> R Num | SP | FP | BP
RegArg	 -> Register | M[Register] | M[Register + Num]
Num	 -> Digit+
-}



