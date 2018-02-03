{-
  Tianyi Zhang
  zhangt73
  Nov 24 2017
  Assignment 5
-}

module DefiniteIntegral where 
import Test.QuickCheck

definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer-> Double
definiteIntegral a b g n = sigmaSum 1 n func
    where
    deltaX = (b - a) / fromIntegral n 
    halfDeltaX = 0.5 * deltaX
    func i = (g (a+deltaX*(i-1)) + g (a+deltaX*i))* halfDeltaX  


sigmaSum :: Integer -> Integer -> (Double -> Double) -> Double
sigmaSum m n f 
  | m > n = 0
  | m <= n = sigmaSum m (n-1) f + f (fromIntegral n)


circleArea :: Double -> Double
circleArea r = 4 * quarterArea  r 
   where 
   quarterArea r  = definiteIntegral 0 r circleFunction 999
   circleFunction x = sqrt (r**2 - x**2)

sphereVolume :: Double -> Double
sphereVolume r = halfSphereVolume r  * 2
   where 
   halfSphereVolume r = definiteIntegral (-r) r areaFunction 12000
   areaFunction r = pi * r** 2




{-
Test Cases:

Function: definiteIntegral
Test Case Number: 1
Input: definiteIntegral 4 8 (\x -> x**3 -2*x +10) 1000
Expected Output: 952 
Actual Output: 952.0001919999995

Function: definiteIntegral
Test Case Number: 2
Input: definiteIntegral 0 0 (\x -> x+1) 30
Expected Output: 0
Actual Output: 0.0

Function: definiteIntegral
Test Case Number: 3
Input: definiteIntegral (-2.2) 6.45 (\x -> 1/ (x**2 +2)) 9999
Expected Output: 1.664837669
Actual Output: 1.6648376622186847
----------------------------------

Function: circleArea
Test Case Number: 1
Input: circleArea 3
Expected Output: 28.27 
Actual Output: 28.273998699561098

Function: circleArea
Test Case Number: 2
Input: circleArea 12.57
Expected Output: 496.39
Actual Output: 496.3811485693648

Function: circleArea
Test Case Number: 3
Input: circleArea 100
Expected Output:  31415.93
Actual Output: 31415.554110623467

===============================================

QuickCheck Test Cases:
----------------------------

Function: definiteIntegral
Property: 
    
prop :: (Double, Double) -> Bool
prop (x,y) = (definiteIntegral x y h 999 + (definiteIntegral y x h 999)) <= 0.5
 
h :: Double -> Double
h x = x**3 -2*x +10
(or any other function)
Actual Test Result: Pass
-----------------------------

Function: definiteIntegral
Property: 
prop :: (Double,Double,Double)  -> Bool
prop (a,b,c) = (definiteIntegral a b (\x -> c) 999) - ((b-a)*c) <= 0.5
Actual Test Result: Pass
-----------------------------

Function: definiteIntegral
Property: 
prop :: (Double,Double,Double)  -> Bool
prop a b c = (definiteIntegral a b (\x -> c) 999) - ((b-a)*c) <= 0.5
Actual Test Result: Pass

-}


