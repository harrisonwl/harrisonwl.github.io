import Base
import Simple
import SimpleParse

-- 3 - -2 - -7
t1 = Subtract (Subtract (Number 3) (Number (-2))) (Number (-7))

testDBZ = evaluate (parseExp "8 / 0")

main'2 = do
  putStrLn "Evaluating the following expression:"
  putStr "  "
  print t1
  putStrLn "Produces the following result:"
  putStr "  "
  print (evaluate t1)

main'3 = do
  test "evaluate" evaluate (parseExp "4")
  test "evaluate" evaluate (parseExp "-5 + 6")
  test "evaluate" evaluate (parseExp "3 - -2 - -7")
  test "evaluate" evaluate (parseExp "3 * (8 + 5)")
  test "evaluate" evaluate (parseExp "1 + 8 * 2")

main = do
  tagged "Eval7" main'2
  tagged "Form12"main'3

  