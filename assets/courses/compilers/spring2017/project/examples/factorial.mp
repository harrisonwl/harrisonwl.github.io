int fac (i : int) { if i==0 then return 1 else return (i * (call fac (i-1))) }
int main () { return (call fac(5)) }

