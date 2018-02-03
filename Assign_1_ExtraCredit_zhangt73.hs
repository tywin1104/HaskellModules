import Data.Complex

cubicQ :: Float -> Float -> Float -> Float 
cubicQ a b c = (3 * a * c - b ** 2) / (9 * a ** 2)

cubicR :: Float -> Float -> Float -> Float -> Float
cubicR a b c d = (9*a*b*c - 27*a**2*d - 2*b**3) / (54*a**3)

complexCubicRoot::RealFloat a => Complex a -> a
complexCubicRoot a 
    | real>=0 = real ** (1/3)
    | otherwise = negate ((negate real)**(1/3))
    where
    real = realPart a

cubicComplexS :: RealFloat a => a -> a -> Complex a
cubicComplexS q r 
    | q**3+r**2>=0 = (complexRoot:+0)
    | otherwise = ((r:+0)+(0:+value))**(1/3)
    where
    complexRoot = complexCubicRoot((r:+0)+(value:+0))
    value = sqrt(abs(q**3+r**2))

cubicComplexT :: RealFloat a => a -> a -> Complex a
cubicComplexT q r 
    | q**3+r**2>=0 = (complexRoot:+0)
    | otherwise = ((r:+0)-(0:+value))**(1/3)
    where
    complexRoot = complexCubicRoot((r:+0)-(value:+0))
    value = sqrt(abs(q**3+r**2))

cubicRealSolution :: Float -> Float -> Float -> Float -> Float
cubicRealSolution a b c d = realPart(s+t) - (b/(3*a))
    where
    q = cubicQ a b c 
    r = cubicR a b c d
    s = cubicComplexS q r 
    t = cubicComplexT q r

-----------------------------------------------------------
-- Test Cases:
tol = 0.01

test_0 :: Bool
test_0 = let
  x = cubicRealSolution 1 0 (-3) 0
  x' = sqrt 3
  in abs(x-x') <= tol
  

test_1 :: Bool
test_1 = let 
  x = cubicRealSolution 1 0 0 8
  x' = -2
  in abs(x-x') <= tol

test_2 :: Bool
test_2 = let 
  x = cubicRealSolution 1 (-6) 11 (-6)
  x' = 3
  in abs(x-x') <= tol

test_3 :: Bool
test_3 = let 
  x = cubicRealSolution 1 (-5) 8 (-4)
  x' = 1 
  in abs(x-x') <= tol

test_4 :: Bool
test_4 = let 
  x = cubicRealSolution 1 5 (-14) 0
  x' = 2
  in abs(x-x') <= tol

test_5 :: Bool
test_5 = let 
  x = cubicRealSolution 1 2 (-9) (-18)
  x' = 3
  in abs(x-x') <= tol
