
f(x) = x * 2

test1 = let x = 3 in 2*x + 5

test2 = 2 * (let x = 3 in x + 5)

test3 = let x = 3 in let y = x*2 in x + y

test4 = let x = 3 in (let y = x*2 in x + y)

f'1(x) = x * 2
f'2 x  = x * 2
f'3 = \x -> x * 2


testLet =
  let fact = \n -> if n == 0 then 1 else n * fact(n-1)
  in fact(10)

testLet3 = let x = x + 1 in x

testLet2 =
  let x = y + 1
      y = 99
  in x * y


testID = id(id)   
-- returns id

testP = let k = 2 in
  let double = \n -> k * n in
    let k = 9 in
      double k


main = return ()
