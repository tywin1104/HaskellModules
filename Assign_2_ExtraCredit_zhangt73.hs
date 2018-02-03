-- Tywin(Tianyi)Zhang zhangt73 Oct 10, 2017 Assignment2 Extra Credit
module VectorSpace where

newtype Vector2 a = Vector2 (a,a)
    deriving (Show,Eq)

newtype Vector3 a = Vector3 (a,a,a)
    deriving (Show,Eq)

newtype Vector4 a = Vector4 (a,a,a,a)
    deriving (Show,Eq)

class VectorSpace v where
    vecZero :: (Num a) => v a
    vecSum :: (Num a) => v a -> v a -> v a
    vecScalarProd :: (Num a)=> a -> v a -> v a
    vecMagnitude :: (Floating a) => v a -> a

instance VectorSpace Vector2 where
    vecZero = Vector2 (0, 0)
    vecSum (Vector2(a1,b1)) (Vector2(a2,b2)) = Vector2(a1+a2, b1+b2)
    vecScalarProd r  (Vector2(a,b)) = Vector2 (r*a, r*b)
    vecMagnitude (Vector2(a,b)) = sqrt(a**2 + b**2)

instance VectorSpace Vector3 where
    vecZero = Vector3 (0, 0, 0)
    vecSum (Vector3(a1,b1,c1)) (Vector3(a2,b2,c2)) = Vector3(a1+a2, b1+b2,c1+c2)
    vecScalarProd r (Vector3 (a,b,c)) = Vector3 (r*a, r*b, r*c)
    vecMagnitude (Vector3(a,b,c)) = sqrt(a**2 + b**2 + c**2)

instance VectorSpace Vector4 where
    vecZero = Vector4 (0, 0, 0, 0)
    vecSum (Vector4(a1,b1,c1,d1)) (Vector4(a2,b2,c2,d2)) = Vector4(a1+a2, b1+b2,c1+c2, d1+d2)
    vecScalarProd r (Vector4 (a,b,c,d)) = Vector4(r*a, r*b, r*c, r*d)
    vecMagnitude (Vector4(a,b,c,d)) = sqrt(a**2 + b**2 + c**2 + d**2)


vecDistance :: (Floating a,VectorSpace v) => v a -> v a -> a
vecDistance vec1 vec2 = vecMagnitude resultVec
    where
    negativeVec2 = vecScalarProd (-1) vec2
    resultVec = vecSum vec1 negativeVec2


vecF :: (Floating a, VectorSpace v)=> v a -> [v a] -> [a]
vecF x y = [vecDistance x vec| vec <- y] 


{-
Test Cases:
===========================
Function: vecScalarProd
Test Case Number: 1
Input: 2 (Vector2(1,2))
Expected Output: Vector2(2,4)
Actual Output: Vector2(2,4) 


Function: vecScalarProd
Test Case Number: 2
Input: (-3) Vector3(1.2,3.4,-5)
Expected Output: Vector3(-3.6,-10.2,15.0)
Actual Output: Vector3 (-3.5999999999999996,-10.2,15.0)


Function: vecScalarProd
Test Case Number: 3
Input: 4 Vector4(2,-2,1,0)
Expected Output: Vector4(8,-8,4,0)
Actual Output: Vector4 (8,-8,4,0)

-----------------------
Function: vecSum
Test Case Number: 1
Input: (Vector3(2,3,4)) (Vector3(1,2,3))
Expected Output: Vector3(3,5,7)
Actual Output: Vector3(3,5,7)

Function: vecSum
Test Case Number: 2
Input: (Vector2(1,-3)) (Vector2(2.2,1.1))
Expected Output: Vector2(3.2, -1.9)
Actual Output: Vector2(3.2, -1.9)

Function: vecSum
Test Case Number: 3
Input: (Vector4(1,0,-1,0)) (Vector4(10,4.2,-2,4))
Expected Output: Vector4 (11.0,4.2,-3.0,4.0)
Actual Output: Vector4 (11.0,4.2,-3.0,4.0)

-------------------------

Function: vecMagnitude
Test Case Number: 1
Input: (Vector3(0,3,4))
Expected Output: 5.0
Actual Output: 5.0

Function: vecMagnitude
Test Case Number: 2
Input: (Vector2(-1,1))
Expected Output: 1.414
Actual Output: 1.41421356237309512

Function: vecMagnitude
Test Case Number: 3
Input: (Vector4(2,3,4,-11))
Expected Output: 12.24745
Actual Output: 12.24744871391589

---------------------------

Function: vecF
Test Case Number: 1
Input: (Vector3(0,0,0)) [Vector3(0,3,4), Vector3(-1,-1,-1), Vector3 (11,-3,-4)]
Expected Output: [5.0,1.732,12.0830]
Actual Output: [5.0,1.7320508075688772,12.083045973594572]

Function: vecF
Test Case Number: 2
Input: (Vector2(1,2)) [Vector2(-2.2,3.3),Vector2(110,-120)]
Expected Output:[3.453983,163.600122]
Actual Output: [3.453983207834109,163.6001222493431]

Function: vecF
Test Case Number: 3
Input: (Vector4(1,-2,0,0)) [Vector4(0,0,0,0),Vector4(9.3,2.8,22,-33)]
Expected Output: [2.236068,40.803554]
Actual Output: [2.23606797749979,40.8035537667983]
-}









