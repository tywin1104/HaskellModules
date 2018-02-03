cubicQ :: Float -> Float -> Float -> Float 
cubicQ a b c = (3 * a * c - b ** 2) / (9 * a ** 2)

cubicR :: Float -> Float -> Float -> Float -> Float
cubicR a b c d = (9*a*b*c - 27*a**2*d - 2*b**3) / (54*a**3)

cubicRoot :: Float -> Float

-- refined cubicRoot function. If a is not a real number, return NaN.
cubicRoot a 
    | a>=0 = a ** (1/3)
    | a<0 = negate ((negate a)**(1/3))
    | otherwise = a


cubicS :: Float -> Float -> Float
cubicS q r = cubicRoot(r + sqrt(q**3 + r**2))

cubicT :: Float -> Float ->Float
cubicT q r = cubicRoot(r - sqrt(q**3 + r**2))

cubicRealSolution :: Float -> Float -> Float -> Float -> Float
cubicRealSolution a b c d = s + t - (b/(3*a))
    where 
    q = cubicQ a b c
    r = cubicR a b c d 
    s = cubicS q r 
    t = cubicT q r
    value = q**3 + r**2
----------------------------------------------------------------
-- Test Cases:
tol = 0.01

test_0 :: Bool
test_0 = 
  let x = cubicRealSolution 1 0 (-3) 0
  in isNaN(x)

test_1 :: Bool
test_1 = let 
  x = cubicRealSolution 1 0 0 8
  x' = -2
  in abs(x-x') <= tol

test_2 :: Bool
test_2 = 
  let x = cubicRealSolution 1 (-6) 11 (-6)
  in isNaN(x)

test_3 :: Bool
test_3 = let 
  x = cubicRealSolution 1 (-5) 8 (-4)
  x' = 1 
  in abs(x-x') <= tol

test_4 :: Bool
test_4 = let 
  x = cubicRealSolution 1 0 0 1
  x' = -1
  in abs(x-x') <= tol

test_5 :: Bool
test_5 = let 
  x = cubicRealSolution 1 (-12) 45 (-50)
  x' = 2
  in abs(x-x') <= tol