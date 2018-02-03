-- Tywin(Tianyi)Zhang zhangt73 Oct 10, 2017 Assignment2 
module VectorSpace where

type Vector = (Double,Double,Double)

vecZero :: Vector
vecZero = (0.0, 0.0, 0.0)

vecScalarProd :: Double -> Vector -> Vector
vecScalarProd r (a,b,c) = (r*a, r*b, r*c)

vecSum :: Vector -> Vector -> Vector
vecSum (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)

vecMagnitude :: Vector -> Double
vecMagnitude (a,b,c) = sqrt(a**2 + b**2 + c**2)

vecDistance :: Vector -> Vector -> Double
vecDistance a b = vecMagnitude resultVector
    where 
    negativeB = vecScalarProd (-1) b 
    resultVector = vecSum a negativeB

vecF :: Vector ->  [Vector] -> [Double]
vecF x y = [vecDistance x vec| vec <- y]

{-
Test Cases:
===========================
Function: vecScalarProd
Test Case Number: 1
Input: 2 (1,2,3)
Expected Output: (2.0,4.0,6.0)
Actual Output: (2.0,4.0,6.0)


Function: vecScalarProd
Test Case Number: 2
Input: 0 (2,21,33)
Expected Output: (0.0,0.0,0.0)
Actual Output: (0.0,0.0,0.0)


Function: vecScalarProd
Test Case Number: 3
Input: (-4) (2,-2,1)
Expected Output: (-8.0,8.0,-4.0)
Actual Output: (-8.0,8.0,-4.0)

-----------------------
Function: vecSum
Test Case Number: 1
Input: (2,3,4) (1,2,3)
Expected Output: (3.0,5.0,7.0)
Actual Output: (3.0,5.0,7.0)

Function: vecSum
Test Case Number: 2
Input: (-2,3,-4) (1,-2,3)
Expected Output: (-1.0,1.0,-1.0)
Actual Output: (-1.0,1.0,-1.0)

Function: vecSum
Test Case Number: 3
Input: (-10.5,3.666,9.999) (1,22,33)
Expected Output: (-9.5,25.666,42.999)
Actual Output: (-9.5,25.666,42.999)

Function: vecSum
Test Case Number: 4
Input: (0,0,0) (0,0,0)
Expected Output: (0.0,0.0,0.0)
Actual Output: (0.0,0.0,0.0)

-------------------------

Function: vecMagnitude
Test Case Number: 1
Input: (0,3,4)
Expected Output: 5.0
Actual Output: 5.0

Function: vecMagnitude
Test Case Number: 2
Input: (-1,-1,-1)
Expected Output: 1.732...
Actual Output: 1.7320508075688772

Function: vecMagnitude
Test Case Number: 3
Input: (11,-3,-4)
Expected Output: 12.0830...
Actual Output: 12.083045973594572

---------------------------

Function: vecF
Test Case Number: 1
Input: (0,0,0) [(0,3,4), (-1,-1,-1), (11,-3,-4)]
Expected Output: [5.0,1.732,12.0830]
Actual Output: [5.0,1.7320508075688772,12.083045973594572]

Function: vecF
Test Case Number: 2
Input: (1,-2,3) [(11,30,40), (1,0,0), (6.5,5.3,-4.0),(1,2,3)]
Expected Output: [49.93, 3.605551, 11.512602,4.0]
Actual Output: [49.92995093127971,3.605551275463989,11.512601791080936,4.0]

Function: vecF
Test Case Number: 3
Input: (7,4,5) [(7,4,5)]
Expected Output: [0.0]
Actual Output: [0.0]
-}
