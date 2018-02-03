-- Tianyi Zhang zhangt73 Oct,26,2017 Assignment3
module Polynomial where 
-- Definition of the algebrativ data type Poly
data Poly = X
        | Coef Integer
        | Sum Poly Poly 
        | Prod Poly Poly
        deriving Show 

{- 
  Function Name: polyValue
  Compute the value of a polynomial function from the input
  Parameters:
    Poly: a defined polynomial function that need to be computed later on
    Integer: the X value of the function so that it can be plugged in 
             to calculate the result
-}
polyValue :: Poly -> Integer -> Integer
polyValue (Coef c) n = c 
polyValue X n = n  
polyValue (Sum a b) n = valueA + valueB
      where
      valueA = polyValue a n 
      valueB = polyValue b n 
polyValue (Prod a b) n = valueA * valueB
      where
      valueA = polyValue a n 
      valueB = polyValue b n 

{- 
  Function Name: polyDegree
  Compute the degree of a given polynomial function
  Parameters:
    Poly: a polynomial function that we want to find the degree of
-}
polyDegree :: Poly -> Integer
polyDegree (Coef c) = 0
polyDegree X = 1
polyDegree (Sum a b) = let primeA = polyDegree a
                           primeB = polyDegree b 
                       in max primeA primeB
polyDegree (Prod a b) = let primeA = polyDegree a
                            primeB = polyDegree b 
                        in primeA + primeB 


{- 
  Function Name: polyDeriv
  Return the derivative of the given polynonial function
  Parameters:
    Poly: a polynomial function that we want to find the derivative of
-}
polyDeriv :: Poly -> Poly
polyDeriv (Coef c)= Coef 0
polyDeriv X = Coef 1
polyDeriv (Sum a b) = Sum primeA primeB
    where 
    primeA = polyDeriv a 
    primeB = polyDeriv b 
polyDeriv (Prod a b) = Sum (Prod primeA b) (Prod a primeB)
    where 
    primeA = polyDeriv a 
    primeB = polyDeriv b 


{-
  Test Cases:
  ------------------------------------
  Function: polyValue
  Test Case Number: 1
  Input: (Sum (Sum (Prod (Coef 2) (Prod X X)) (Prod (Coef 3) (X))) (Coef 4)) 3
  Expected Output: 31
  Actual Output: 31
  
  Function: polyValue
  Test Case Number: 2
  Input: (Prod (Coef(-9)) (Prod (Coef(-1)) (Prod X (Prod X X)))) (-2)
  Expected Output: -72
  Actual Output: -72

  Function: polyValue
  Test Case Number: 3
  Input: (Prod (Sum X (Coef 2)) (Sum X (Coef 3))) 1
  Expected Output: 12
  Actual Output: 12
  -------------------------------------
  Function: polyDegree
  Test Case Number: 1
  Input: Sum (Sum (Prod (Coef 2) (Prod X X)) (Prod (Coef 3) (X))) (Coef 4) 
  Expected Output: 2
  Actual Output: 2

  Function: polyDegree
  Test Case Number: 2
  Input: Prod X (Prod X X)
  Expected Output: 3
  Actual Output: 3

  Function: polyDegree
  Test Case Number: 3
  Input: Sum (Prod (Coef 2) X) (Prod X (Prod X (Prod X (Coef 0))))
  Expected Output: 3 (>=1)
  Actual Output: 3
  ----------------------------------------

  Function: polyDeriv
  Test Case Number: 1
  Input: Sum (Sum (Prod (Coef 2) (Prod X X)) (Prod (Coef 3) (X))) (Coef 4) 
  Expected Output:  4x+3 after simplify
  Actual Output: Sum (Sum (Sum (Prod (Coef 0) (Prod X X)) (Prod (Coef 2) (Sum (Prod (Coef 1) X) (Prod X (Coef 1))))) (Sum (Prod (Coef 0) X) (Prod (Coef 3) (Coef 1)))) (Coef 0)
  
  Function: polyDeriv
  Test Case Number: 2
  Input: Prod X (Prod X X) 
  Expected Output:  2x^2 after simplify
  Actual Output: Sum (Prod (Coef 1) (Prod X X)) (Prod X (Sum (Prod (Coef 1) X) (Prod X (Coef 1))))
  
  Function: polyDeriv
  Test Case Number: 3
  Input: Prod (Sum X (Coef 2)) (Sum X (Coef 3))
  Expected Output:  2x+5 after simplify
  Actual Output: Sum (Prod (Sum (Coef 1) (Coef 0)) (Sum X (Coef 3))) (Prod (Sum X (Coef 2)) (Sum (Coef 1) (Coef 0)))
-}


