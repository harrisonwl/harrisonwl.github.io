int fac (i : int) { if i==0 then return 1 else return (i * (call fac (i-1))) }
int main () { return (call fac(5)) }

/*
int fac (i : int) { if i==0 then return 1 else return (i * (call fac (i-1))) }
int main (i : float) { return (i + i) }
  
int fac (fac_i : int) { if fac_i==0 then return 1 else return (fac_i * (call fac (fac_i-1))) }
int main (main_i : float) { return (main_i + main_i) }

ex1 = Program fds
  where
    fds = [fac,main]
    fac = FunDecl INT "fac" [Decl "i" INT] []
             (IfThenElse
                (BinOp EQ (Var "i") (ICon 0))
                (Return (ICon 1))
                (Return (BinOp Times (Var "i") (FunCall "fac" [BinOp Sub (Var "i") (ICon 1)]))))
    main = FunDecl INT "main" [] [] (Return (FunCall "fac" [ICon 5]))
*/
