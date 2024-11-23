doubleMe :: (Num a) => a -> a
doubleMe x = x + x

doubleUs :: (Num a) => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

doubleSmallNumber' :: (Ord a, Num a) => a -> a
doubleSmallNumber' x = doubleSmallNumber x + 1

conanO'Brien :: String
conanO'Brien = "It's a-me, Conan O'Brien!"

fizzBuzz :: (Integral a) => [a] -> [String]
fizzBuzz xs = [if x `mod` 5 == 0 && x `mod` 3 == 0 then "fizzbuzz" else if x `mod` 5 == 0 then "buzz" else if x `mod` 3 == 0 then "fizz" else "ERROR" | x <- xs, x `mod` 3 == 0 || x `mod` 5 == 0]

listCombos :: (Num a) => [a] -> [a] -> [a]
listCombos xs ys = [x + y | x <- xs, y <- ys]

removeNonUpper :: [Char] -> [Char]
removeNonUpper st = [c | c <- st, c `elem` ['A' .. 'Z']]

-- Triangle Example p.21--
rightTriangles :: [(Integer, Integer, Integer)]
rightTriangles =
  [ (a, b, c)
    | c <- [1 .. 10],
      a <- [1 .. c],
      b <- [1 .. a],
      a ^ 2 + b ^ 2 == c ^ 2
  ]

rightTriangles' :: [(Integer, Integer, Integer)]
rightTriangles' =
  [ (a, b, c)
    | c <- [1 .. 10],
      a <- [1 .. c],
      b <- [1 .. a],
      a ^ 2 + b ^ 2 == c ^ 2,
      a + b + c == 24
  ]

factorial :: Integer -> Integer
factorial n = product [1 .. n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r