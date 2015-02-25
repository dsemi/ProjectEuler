module Euler.Problem235
( problem235
) where

-- Given is the arithmetic-geometric sequence u(k) = (900-3k)r^(k-1).
-- Let s(n) = Σk=1...nu(k).

-- Find the value of r for which s(5000) = -600,000,000,000.

-- Give your answer rounded to 12 places behind the decimal point.

-- sum 900r^(k-1) - 3kr^(k-1)

-- 900*(1-r^n)/(1-r) - (3*n*r^(n+1) - 3*(n+1)*r^n + 3)/(r-1)^2

-- solve 900*(1-r^5000)/(1-r) - (3*5000*r^(5001) - 3*5001*r^5000 + 3)/(r-1)^2 = -600000000000

