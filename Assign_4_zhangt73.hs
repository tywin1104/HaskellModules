{-
  Tianyi Zhang
  zhangt73
  Assignment4
  Nov 16 2017 
-}
module PolynomialList where 
import Test.QuickCheck

data Poly = 
    X
  | Coef Integer
  | Sum Poly Poly
  | Prod Poly Poly
  deriving Show   

getPolyList :: FilePath -> IO [Integer]
getPolyList path = do info <- readFile path
                      return (parse info)

parse :: String -> [Integer]
parse info = let 
    strings = lines info
    ints = map read strings ::[Integer]
    in ints

polyListValue :: [Integer] -> Integer -> Integer
polyListValue poly x 
    |length poly > 2 = (x* polyListValue (tail poly) x) + poly!!0 
    |otherwise =  poly!!0 + x* last poly


polyListDegree :: [Integer] -> Integer 
polyListDegree poly 
  |length poly>0 = toInteger (length poly -1)

 
polyListDeriv :: [Integer] -> [Integer] 
polyListDeriv poly 
    |length poly > 1 = let indexTuple = zip poly [0..]
                           derivList = [a * b | (a,b) <- indexTuple]
                       in  drop 1 derivList
    |otherwise = [0] 

polyListSum :: [Integer] -> [Integer] -> [Integer] 
polyListSum [] [] = []
polyListSum [] ys = polyListSum  (replicate (length ys) 0) ys
polyListSum xs [] = polyListSum xs (replicate (length xs) 0)
polyListSum (x:xs) (y:ys) = (x+y) : polyListSum xs ys 

polyListProd:: [Integer] -> [Integer] -> [Integer]
polyListProd _ [0] = [0] 
polyListProd [0] _ = [0]
polyListProd xs ys = let txs = zip xs [0..]
                         tys = zip ys [0..]
                         middleLists = [prod x y | x <- txs, y <- tys]   
                     in  foldr polyListSum [] middleLists

prod ::  (Integer,Integer) -> (Integer,Integer) -> [Integer]
prod (a1,b1) (a2,b2) = produce (b1+b2) 0 ++ [a1*a2] 
    where 
    produce :: Integer -> Integer -> [Integer]
    produce 0 x = []
    produce num x = x : produce (num-1) x

polyListToPoly :: [Integer] -> Poly
polyListToPoly poly = let tList = zip poly [0..]
                          polys = map transform tList
                      in  addAll polys

addAll :: [Poly]-> Poly
addAll [] = Coef 0
addAll (p:ps) = Sum p (addAll ps)

transform :: (Integer,Integer) -> Poly 
transform (c, 0) = Coef c 
transform (num, power)  
  | power ==1 = Prod (Coef num) X
  | otherwise = Prod X (transform (num,power-1))

polyToPolyList :: Poly -> [Integer]
polyToPolyList (Coef c) = [c]
polyToPolyList X = [0,1]
polyToPolyList (Sum a b) = polyListSum (polyToPolyList a) (polyToPolyList b)
polyToPolyList (Prod a b) = polyListProd (polyToPolyList a) (polyToPolyList b)

{-
Test Cases: 

--------------------------------------------------------
Function: getPolyList
Test Case Number: 1
Input: poly.txt (in which one Integer per line: 0 4 3 3)
Expected Output: [0,4,3,3]
Actual Output: [0,4,3,3]

Function: getPolyList
Test Case Number: 2
Input: poly.txt (in which one Integer per line: 1 0 0 1 1)
Expected Output: [1,0,0,1,1]
Actual Output: [1,0,0,1,1]

Function: getPolyList
Test Case Number: 3
Input: poly.txt (in which one Integer per line: -2 3 0 1)
Expected Output: [-2,3,0,1]
Actual Output: [-2,3,0,1]
--------------------------------------------------------

Function: polyListValue 
Test Case Number: 1
Input: [-3,9,2] 0 
Expected Output: -3
Actual Output: -3

Function: polyListValue 
Test Case Number: 2
Input: [1,4,2,3] 8 
Expected Output: 1697
Actual Output: 1697

Function: polyListValue 
Test Case Number: 3
Input: [-3,9,2] (-2) 
Expected Output: -13
Actual Output: -13

--------------------------------------------------------
Function: polyListDegree 
Test Case Number: 1
Input: [-3,9,2]  
Expected Output: 2
Actual Output: 2

Function: polyListDegree 
Test Case Number: 2
Input: [1,2,2,2,2,9,0,9]  
Expected Output: 7
Actual Output: 7

Function: polyListDegree 
Test Case Number: 3
Input: []  
Expected Output: undefined
Actual Output: undefined
-------------------------------------------------

Function: polyListDeriv
Test Case Number: 1
Input: [-3,9,2]  
Expected Output: [9,4]
Actual Output: [9,4] 

Function: polyListDeriv
Test Case Number: 2
Input: [-2,-9,0,5]  
Expected Output: [-9,0,15]
Actual Output: [-9,0,15]

Function: polyListDeriv
Test Case Number: 3
Input: [0,0,0,0,1]  
Expected Output: [0,0,0,4]
Actual Output: [0,0,0,4] 
-------------------------------------------------- 
Function: polyListSum
Test Case Number: 1
Input: [-3,9,2] [2,0,1]  
Expected Output: [-1,9,3] 
Actual Output: [-1,9,3]

Function: polyListSum
Test Case Number: 2
Input: [-2,1,0,2] [99,2]  
Expected Output: [97,3,0,2] 
Actual Output: [97,3,0,2]

Function: polyListSum
Test Case Number: 3
Input: [0,1] [-1,0,-1,0,1]  
Expected Output: [-1,1,-1,0,1]
Actual Output: [-1,1,-1,0,1]
--------------------------------------------------
Function: polyListProd 
Test Case Number: 1
Input: [1,5,1] [15,-10,3]  
Expected Output: [15,65,-32,5,3] 
Actual Output: [15,65,-32,5,3] 

Function: polyListProd 
Test Case Number: 2
Input: [5,-1] [9,-19,1]
Expected Output: [45,-104,24,-1]
Actual Output: [45,-104,24,-1]  
 
Function: polyListProd 
Test Case Number: 3
Input: [2,1] [6,1]  
Expected Output: [12,8,1]
Actual Output: [12,8,1]
----------------------------------------------
Function: polyListToPoly
Test Case Number: 1
Input: [2,1]  
Expected Output: 2 + x 
Actual Output: Sum (Coef 2) (Sum (Prod (Coef 1) X) (Coef 0))

Function: polyListToPoly
Test Case Number: 2
Input: [4,-3,0,1]  
Expected Output: 4-3x+x^3
Actual Output: Sum (Coef 4) (Sum (Prod (Coef (-3)) X) (Sum (Prod X (Prod (Coef 0) X)) (Sum (Prod X (Prod X (Prod (Coef 1) X))) (Coef 0))))

Function: polyListToPoly
Test Case Number: 3
Input: [0]  
Expected Output: Coef 0
Actual Output: Sum (Coef 0) (Coef 0) Okay
----------------------------------------------------
Function: polyToPolyList
Test Case Number: 1
Input: Sum (Coef 4) (Sum (Prod (Coef (-3)) X) (Sum (Prod X (Prod (Coef 0) X)) (Sum (Prod X (Prod X (Prod (Coef 1) X))) (Coef 0)))) 
Expected Output: [4,-3,0,1] 
Actual Output: [4,-3,0,1]

Function: polyToPolyList
Test Case Number: 2
Input: Sum (Coef 2) (Sum (Prod (Coef 1) X) (Coef 0))
Expected Output: [2,1] 
Actual Output: [2,1]

Function: polyToPolyList
Test Case Number: 3
Input: Coef 0
Expected Output: [0]
Actual Output: [0] 
======================================================

QUICK CHECK TEST CASES:

Function: polyListValue
Property: 
    prop :: Integer -> Bool
    prop x = polyListValue [x,1,-2] x == x+x-2*x^2
Actual Test Result: Pass 
-------------------------
Function: polyListDegree
Property: 
    prop :: Integer -> Bool
    prop x = polyListDegree [0,0,x,1] == polyListDegree [0,x,0,1] 
Actual Test Result: Pass 
-------------------------
Function: polyListDeriv
Property: 
    prop :: [Integer] -> Bool
    prop xs =length xs >0 || polyListDegree (polyListDeriv xs) == polyListDegree xs - 1 
Actual Test Result: Pass 
-------------------------
Function: polyListSum
Property: 
    prop :: ([Integer],[Integer]) -> Bool
    prop (x,y) = polyListSum x y == polyListSum y x
Actual Test Result: Pass 
------------------------
Function: polyListProd 
Property: 
    prop :: ([Integer],[Integer]) -> Bool
    prop (x,y) = polyListProd x y == polyListProd y x
Actual Test Result: Pass 

========================= 
-}




