module ThreadLanguage where

    -----------------------------------------------------
    --- Syntax for expressions, events, and processes ---
    -----------------------------------------------------

type Loc  = String

data Exp     = Lit Int | Address Loc | GetPID
data Event   = Write Loc Exp
             | Print String Exp
             | Sleep 
             | Fork Process
             | Broadcast Exp
             | Receive Loc
             | Psem
             | Vsem
             | Kill Exp
             | Inc Loc --- Just added for examples

data Process = Process Event Process

showProc 0 _             = "..."
showProc i (Process e p) = show e ++ " ; " ++ showProc (i-1) p

instance Show Process where
    show p = showProc 5 p

instance Show Exp where
    show (Address n)  = n
    show (Lit i)      = show i
    show GetPID	      = "pid"

instance Show Event where
    show (Write l e)	= l ++ ":=" ++ show e
    show (Print msg e)	= "print '" ++ msg ++ "' " ++ show e
    show Psem		= "P"
    show Vsem		= "V"
    show Sleep		= "sleep"
    show (Fork p)	= "fork(" ++ show p ++ ")"
    show (Broadcast e)  = "bcast(" ++ show e ++ ")"
    show (Receive name)	= "recv(" ++ name ++ ")"
    show (Kill e)       = "kill(" ++ show e ++ ")"
    show (Inc l)        = "inc(" ++ l ++ ")"
