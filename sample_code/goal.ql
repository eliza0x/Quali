main = print (double 2)

plus n m = if (iszero n)
              m
              (plus (succ n) (pred m))

double n = if (not (iszero n))  
              (2 + (double (n - 1)))
              0
