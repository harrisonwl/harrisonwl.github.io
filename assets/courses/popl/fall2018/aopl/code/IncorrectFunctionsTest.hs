import Base
import IncorrectFunctions
import IncorrectFunctionsParse

teste1 = let add = \a -> (\b -> b + a) in add 3 2

testE2 = parseExp ("var add = function(a) { function(b) { a + b } };"++
                   "    add(3)(2)")

testP2 = parseExp ("var f = function(x) { x * x };"++
                   "  f(10)")

main = do
  (test "execute" execute testE2)
  (test "execute" execute testP2)
  --tagged "InFunOut1" (test "execute" execute testE2)
  --tagged "InFunOut2" (test "execute" execute testP2)
  
