-- Tianyi Zhang  zhangt73 Oct 30 2017  Assignment 3 Extra Credit
module Polynomial where 

data Poly a = X
    | Coef a 
    | Sum (Poly a) (Poly a)
    | Prod (Poly a) (Poly a)
    deriving Show

polyValue :: Num a => Poly a -> a -> a
polyValue (Coef c) n = c 
polyValue X n = n  
polyValue (Sum a b) n = polyValue a n + polyValue b n
polyValue (Prod a b) n = polyValue a n * polyValue b n 


simplifyAux :: (Num a, Eq a) => Poly a -> Poly a 
simplifyAux (Coef c) = Coef c 
simplifyAux X = X
simplifyAux (Sum a b) = Sum (simplifyAux a) (simplifyAux b)
simplifyAux (Prod (Coef 0) _) = Coef 0 
simplifyAux (Prod _ (Coef 0)) = Coef 0 
simplifyAux (Prod a b) = Prod (simplifyAux a) (simplifyAux b)

simplify :: (Num a, Eq a) => Poly a -> Poly a    
simplify p  
  | polyDegreeAux p == polyDegreeAux (simplifyAux p) = p 
  | otherwise = simplify (simplifyAux p)        
 
polyDegreeAux :: (Num a, Eq a) => Poly a -> Integer
polyDegreeAux (Coef c) = 0
polyDegreeAux X = 1
polyDegreeAux (Sum a b) = let 
                         primeA = polyDegreeAux a
                         primeB = polyDegreeAux b 
                         in max primeA primeB
polyDegreeAux (Prod a b) = let 
                         primeA = polyDegreeAux a
                         primeB = polyDegreeAux b 
                         in primeA + primeB 

polyDegree:: (Num a, Eq a) => Poly a -> Integer 
polyDegree p = polyDegreeAux (simplify p)


polyDeriv :: Num a => Poly a -> Poly a
polyDeriv (Coef c)= Coef 0
polyDeriv X = Coef 1
polyDeriv (Sum a b) = Sum (polyDeriv a) (polyDeriv b)
polyDeriv (Prod a b) = Sum (Prod primeA b) (Prod a primeB)
    where 
    primeA = polyDeriv a 
    primeB = polyDeriv b 

polyNewtonAux:: (Fractional a, Ord a) => Poly a -> a -> a
polyNewtonAux p s = s - (valueP/primeValue)
    where 
    valueP = polyValue p s
    primeP = polyDeriv p 
    primeValue = polyValue primeP s

polyNewton :: (Fractional a, Ord a) => Poly a -> a -> a 
polyNewton p n 
  | abs (result - (polyNewtonAux p result)) <= tol = result
  | otherwise = polyNewton p result
    where 
    tol = 0.01
    result = polyNewtonAux p n


-- polyAsList :: (Num a, Eq a) => Poly a -> [a]


{-
  Test Cases:
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
---------------------------
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
  Expected Output: 1
  Actual Output: 1
  --------------------------

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
  ------------------------
  Function: polyNewton
  Test Case Number: 1
  Input: (Sum (Prod (Coef(4)) X) (Coef 3)) 0 
  Expected Output: -0.75
  Actual Output: -0.75

  Function: polyNewton
  Test Case Number: 2
  Input: (Prod (Coef (-1)) (Prod (Sum X (Coef 2)) (Sum X (Coef 3)))) 100
  Expected Output: -2.0
  Actual Output: -1.993183219051754

  Function: polyNewton
  Test Case Number: 3
  Input: (Sum (Sum (Prod (Coef 2) (Prod X X)) (Prod (Coef 3) (X))) (Coef 4)) 3
  Expected Output: \ no solution
  Actual Output: \ no solution
-}




