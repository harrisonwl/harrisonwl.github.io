
fact = \n -> if n == 0 then 1 else n * fact(n-1)

twos = 2 : twos

numbers = 0 : [ n + 1 | n <- numbers ]

a = 1 + 3 * a

inf = inf

fix g = g (fix g)

g_twos l = 2 : l

g_numbers ns = 0 : [ n + 1 | n <- ns ]

g_fact = \f -> \n -> if n == 0 then 1 else n * f(n-1)

fact'1 = fix g_fact

id x = x

main = return ()
