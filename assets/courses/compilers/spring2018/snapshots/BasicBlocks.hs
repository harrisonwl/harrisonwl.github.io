module BasicBlocks where

import CodeGen
import Common
import ImpParser
import ThreeAddrSyntax
import Control.Monad.Identity
import Control.Monad.State

controlflow :: ThreeAddr -> Bool
controlflow (Jmp _)    = True
controlflow (BrZ _ _)  = True
controlflow (BrNZ _ _) = True
controlflow (BrGT _ _) = True
controlflow (BrGE _ _) = True
controlflow (Call _)   = True
controlflow Ret        = True
controlflow Exit       = True
controlflow _          = False

label (Label _) = True
label _         = False

blockify :: [ThreeAddr] -> [ThreeAddr] -> [[ThreeAddr]]
blockify [] acc       = []
blockify (tc:tcs) acc | controlflow tc = (reverse $ tc : acc) : blockify tcs []
                      | label tc       = case acc of
                                              [] -> blockify tcs [tc]
                                              _  -> reverse acc : blockify tcs [tc]
                      | otherwise      = blockify tcs (tc : acc)
type Block = (Int,[(Int,ThreeAddr)])

genblocks :: [[ThreeAddr]] -> [Block]
genblocks = undefined

type T = StateT (Int,Int) Identity

enblock :: Int -> Int -> [[ThreeAddr]] -> [Block]
enblock bct ict []           = []
enblock bct ict (tacb:tacbs) = (bct,ltacb) : enblock (bct+1) ict' tacbs
  where
    (ltacb,ict') = help tacb ict

enblockT :: [[ThreeAddr]] -> T [Block]
enblockT []           = return []
enblockT (tacb:tacbs) = do
  bl    <- newblocklabel
  ltacb <- helpT tacb
  ltacbs <- enblockT tacbs
  return ((bl,ltacb) : ltacbs)

helpT :: [ThreeAddr] -> T [(Int,ThreeAddr)]
helpT []         = return []
helpT (tac:tacs) = do
  il <- newinstrlabel
  ltacs <- helpT tacs
  return ((il,tac):ltacs)

help :: [ThreeAddr] -> Int -> ([(Int,ThreeAddr)],Int)
help [] icnt         = ([],icnt)
help (tac:tacs) icnt = ((icnt,tac) : ltacs,icnt')
  where
    (ltacs,icnt') = help tacs (icnt+1)

newinstrlabel :: T Int
newinstrlabel = do
  (bct,ict) <- get
  put (bct,ict+1)
  return ict

newblocklabel :: T Int
newblocklabel = do
  (bct,ict) <- get
  put (bct+1,ict)
  return bct

                                                                
bb :: FilePath -> IO [[ThreeAddr]]
bb imp = do
  impprog <- parseImp imp
  let ThreeAddrProg tac = runM $ compileImp impprog
  return $ blockify tac []

foobar_tac :: [ThreeAddr]
foobar_tac = [
  Label 0,
  Mov (Reg "0") (Literal 99),
  Mov (Reg "x") (Immediate (Reg "0")),
  Label 1,Mov (Reg "1") (Literal 0),
  Sub (Reg "2") (Immediate (Reg "x")) (Immediate (Reg "1")),
  BrNZ (Reg "2") (Literal 3),
  Mov (Reg "2") (Literal 0),
  Jmp (Literal 4),Label 3,
  Mov (Reg "2") (Literal 1),
  Label 4,
  BrZ (Reg "2") (Literal 2),
  Mov (Reg "3") (Literal 1),
  Sub (Reg "4") (Immediate (Reg "x")) (Immediate (Reg "3")),
  Mov (Reg "x") (Immediate (Reg "4")),
  Jmp (Literal 1),
  Label 2,
  Exit]

foobar_bbs :: [[ThreeAddr]]
foobar_bbs = [
  [Label 0,
   Mov (Reg "0") (Literal 99),
   Mov (Reg "x") (Immediate (Reg "0"))],
  [Label 1,Mov (Reg "1") (Literal 0),
   Sub (Reg "2") (Immediate (Reg "x")) (Immediate (Reg "1")),
   BrNZ (Reg "2") (Literal 3)],
  [Mov (Reg "2") (Literal 0),
   Jmp (Literal 4)],
  [Label 3,
   Mov (Reg "2") (Literal 1)],
  [Label 4,
   BrZ (Reg "2") (Literal 2)],
  [Mov (Reg "3") (Literal 1),
   Sub (Reg "4") (Immediate (Reg "x")) (Immediate (Reg "3")),
   Mov (Reg "x") (Immediate (Reg "4")),
   Jmp (Literal 1)],
  [Label 2,
   Exit]]

foobar_blocks :: [(Int, [(Int, ThreeAddr)])]
foobar_blocks =
  [(0,[(0,Label 0),
       (1,Mov (Reg "0") (Literal 99)),
       (2,Mov (Reg "x") (Immediate (Reg "0")))]),
   (1,[(3,Label 1),
       (4,Mov (Reg "1") (Literal 0)),
       (5,Sub (Reg "2") (Immediate (Reg "x")) (Immediate (Reg "1"))),
       (6,BrNZ (Reg "2") (Literal 3))]),
   (2,[(7,Mov (Reg "2") (Literal 0)),
       (8,Jmp (Literal 4))]),
   (3,[(9,Label 3),
       (10,Mov (Reg "2") (Literal 1))]),
   (4,[(11,Label 4),
       (12,BrZ (Reg "2") (Literal 2))]),
   (5,[(13,Mov (Reg "3") (Literal 1)),
       (14,Sub (Reg "4") (Immediate (Reg "x")) (Immediate (Reg "3"))),
       (15,Mov (Reg "x") (Immediate (Reg "4"))),
       (16,Jmp (Literal 1))]),
   (6,[(17,Label 2),
       (18,Exit)])]


