module Simple where
import Base

data Exp = Number     Int
         | Add        Exp Exp
         | Subtract   Exp Exp
         | Multiply   Exp Exp
         | Divide     Exp Exp
       deriving Show

evaluate :: Exp -> Int
evaluate (Number i)      = i
evaluate (Add a b)       = evaluate a + evaluate b
evaluate (Subtract a b)  = evaluate a - evaluate b
evaluate (Multiply a b)  = evaluate a * evaluate b
evaluate (Divide a b)    = evaluate a `div` evaluate b
