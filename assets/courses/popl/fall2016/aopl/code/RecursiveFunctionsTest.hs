import Base
import Prelude hiding (LT, GT, EQ)
import RecursiveFunctions
import RecursiveFunctionsParse


facvar = parseExp ("var fac = function(n) { if (n==0) 1 else n * fac(n-1) };" ++
                   "fac(5)")

facrec = parseExp ("rec fac = function(n) { if (n==0) 1 else n * fac(n-1) };" ++
                   "fac(5)")
