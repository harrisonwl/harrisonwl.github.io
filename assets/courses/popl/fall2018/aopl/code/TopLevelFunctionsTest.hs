import Base
import Prelude hiding (LT, GT, EQ)
import TopLevelFunctions
import TopLevelFunctionsParse

p1 = parseExp "function power(n, m) { if (m == 0) 1 else n*power(n, m-1) } power(3, 4)"

fac = parseExp "function fac(n) { if (n == 0) 1 else n*fac(n-1) } fac(5)"

{-

Program
    [("power", Function ["n","m"]
       (If (Binary EQ (Variable "m") (Literal 0))
             (Literal 1)
             (Binary Mul (Variable "n")
                         (Call "power" [Variable "n",
                                        Binary Sub (Variable "m") (Literal 1)]))))]
    (Call "power" [Literal 3,Literal 4])

-}

main = do
  tagged "Top22" (test "execute" execute p1)
  
