-- Recursion function of factoriel with if then else
fac n = 
    if n <= 1 then 1 else n * fac(n-1)

-- Recursion function of factoriel with guards
fac2 n 
    | n <= 1 = 1
    | otherwise  = n * fac2(n-1)

--  Recursion function of factoriel with pattern matching
fac3 1 = 1
fac3 n = n * fac(n-1) 

-- Recursion function of factoriel with Accumulators
fac4 n = aux n 1
  where
    aux n acc
      | n <= 1 = acc
      | otherwise  = aux (n-1) (n*acc)

main = do
    print $ fac 10 -- Function Application
    print $ fac2 10 -- Function Application
    print $ fac3 10 -- Function Application
    print $ fac4 150 -- Function Application