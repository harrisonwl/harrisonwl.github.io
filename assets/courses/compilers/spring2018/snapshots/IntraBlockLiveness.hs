module IntraBlockLiveness where

import Common
import ThreeAddrSyntax
import Data.List

type Block = [ThreeAddr]

-- data Register   = Reg String | SP | FP | BP deriving (Eq,Show)
-- data Arg        = Immediate Register | Literal Word deriving Show

arg2reg (Immediate r) = [r]
arg2reg (Literal _)   = []

defuse :: ThreeAddr -> ([Register],[Register])
defuse tac = case tac of
   Mov rdst arg         -> ([rdst], arg2reg arg)
   Load rdst rsrc       -> ([rdst],[rsrc])
   Store rdst rsrc      -> undefined 
   Add rdst arg1 arg2   -> undefined 
   Sub rdst arg1 arg2   -> ([rdst], arg2reg arg1 ++ arg2reg arg2)
   Div rdst arg1 arg2   -> undefined 
   Mul rdst arg1 arg2   -> undefined 
   Negate rdst arg      -> undefined 
   Equal rdst arg1 arg2 -> undefined 
   LogNot rdst arg      -> undefined 
   GThan rdst arg1 arg2 -> undefined 
   Jmp arg              -> undefined 
   BrZ rtst arg         -> ([],rtst : arg2reg arg)
   BrNZ rtst arg        -> undefined 
   BrGT rtst arg        -> undefined 
   BrGE rtst arg        -> undefined 
   Label _              -> ([],[])
   Read rdst            -> undefined 
   Write arg            -> undefined 
   Call arg             -> undefined 
   Ret                  -> undefined 
   Exit                 -> undefined 

block1 = [Label 0,
          Mov (Reg "0") (Literal 99),
          Mov (Reg "x") (Immediate (Reg "0")),
          Mov (Reg "1") (Literal 0),
          Sub (Reg "2") (Immediate (Reg "x")) (Immediate (Reg "1")),
          Mov (Reg "2") (Literal 0),
          Mov (Reg "2") (Literal 1),
          Mov (Reg "3") (Literal 1),
          Sub (Reg "4") (Immediate (Reg "x")) (Immediate (Reg "3")),
          Mov (Reg "x") (Immediate (Reg "4")),
          BrZ (Reg "2") (Literal 2)]

block2 = [
         Sub (Reg "4") (Immediate (Reg "x")) (Immediate (Reg "3")),
         Mov (Reg "x") (Immediate (Reg "4")),
         BrZ (Reg "2") (Literal 2)]

--
result2 :: [[Register]] -- liveness liveout0 block2
result2 = [
  [Reg "2", Reg "x", Reg "3"],
--         Sub (Reg "4") (Immediate (Reg "x")) (Immediate (Reg "3")),
  [Reg "2",Reg "4"],
--         Mov (Reg "x") (Immediate (Reg "4")),
  [Reg "2",Reg "x"],
--         BrZ (Reg "2") (Literal 2)]
  [Reg "2",Reg "x"]
  ]

result2' = [
  [Reg "2",Reg "x",Reg "3"],
  [Reg "2",Reg "4"],
  [Reg "2",Reg "x"],
  [Reg "2",Reg "x"]]

liveness :: [Register] -> [ThreeAddr] -> [[Register]]
liveness liveout b = reverse $ blocklive (nub liveout) (reverse b)

blocklive liveout []       = [liveout]
blocklive liveout (tac:cs) = liveout : blocklive liveout' cs
  where
    (defs,uses) = defuse tac
    liveout'    = (liveout \\ defs) `union` uses

liveout0 = [Reg "2",Reg "x"]
