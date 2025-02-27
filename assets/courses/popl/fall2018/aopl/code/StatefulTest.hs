import Base
import Stateful
import StatefulParse

mul10 addr mem =
  let IntV n = access addr mem in
    update addr (IntV (10 * n)) mem

testMul10 = mul10 1 [IntV 3, IntV 4, IntV 5, IntV 6]

mul10 :: Int -> Memory -> Memory

t1 = parseExp ("var x = mutable 3;"++
     "var y = mutable true;"++
     "if (@y) { x = @x + 1 } else { x };"++
     "@x")

t2 = parseExp ("var x = mutable 3;"++
     "var y = mutable 7;"++
     "x = @x + @y;"++
     "@x * @y")

main'2 = do
  print testMul10

main = do
  tagged "Upda11" main'2
  test "evaluate" execute t1
  test "evaluate" execute t2
