lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck pal"

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, x2 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x : _) = x

head'' :: [a] -> a -- head' above is syntactic sugar for this
head'' xs = case xs of
  [] -> error "Can't call head on an empty list, dummy!"
  (x : _) -> x

tell :: (Show a) => [a] -> String
tell [] = "Empty List"
tell [x] = "1) " ++ show x
tell [x, y] = tell [x] ++ " 2) " ++ show y
tell (x : y : _) = "THIS IS A REALLY LONG LIST SO HERE'S THE FIRST TWO ELEMENTS: " ++ tell [x, y]

firstLetter :: String -> String
firstLetter "" = "EMPTY STRING DAWG!"
firstLetter all@(x : xs) = "THE FIRST CHARACTER OF " ++ all ++ " IS " ++ [x] -- @ Lets you keep a reference to the original passed value

bmiTell :: Double -> String
bmiTell bmi
  | bmi <= 18.5 = "You're Underweight!"
  | bmi <= 25.0 = "Good BMI Homie"
  | bmi <= 30.0 = "Little Overweight Buddy"
  | otherwise = "BIG CHUNGUS"

bmiTell' :: Double -> Double -> String
bmiTell' height weight
  | height / weight ^ 2 <= 18.5 = "You're Underweight!"
  | height / weight ^ 2 <= 25.0 = "Good BMI Homie"
  | height / weight ^ 2 <= 30.0 = "Little Overweight Buddy"
  | otherwise = "BIG CHUNGUS"

bmiTell'' :: Double -> Double -> String
bmiTell'' height weight
  | bmi <= skinny = "You're Underweight!"
  | bmi <= normal = "Good BMI Homie"
  | bmi <= fat = "Little Overweight Buddy"
  | otherwise = "BIG CHUNGUS"
  where
    bmi = height / weight ^ 2
    skinny = 18.5
    normal = 25.0
    fat = 30.0

max' :: (Ord a) => a -> a -> a
max' a b
  | a <= b = b
  | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a == b = EQ
  | a <= b = LT
  | otherwise = GT

niceGreeting :: String
niceGreeting = "HEY NICE TO SEE YA, "

meanGreeting :: String
meanGreeting = "GET OUT MY FACE YOU PIECE OF SHIT, "

greet :: String -> String
greet "Burt" = niceGreeting ++ " Burt!"
greet "Eggward" = niceGreeting ++ " Eggward!"
greet name = meanGreeting ++ name ++ "!"

initials :: String -> String -> String
initials firstName lastName = [f] ++ "." ++ [l] ++ "."
  where
    (f : _) = firstName
    (l : _) = lastName

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where
    bmi weight height = weight / height ^ 2

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

calcBmis'' :: [(Double, Double)] -> [Double]
calcBmis'' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
   in sideArea + 2 * topArea

describeList :: [a] -> String
describeList ls =
  "THE LIST IS" ++ case ls of
    [] -> " EMPTY."
    [x] -> " A SINGLETON LIST."
    x -> " A LONG LIST."

describeList' :: [a] -> String
describeList' ls =
  "THE LIST IS" ++ what ls
  where
    what [] = " EMPTY."
    what [x] = " A SINGLETON LIST."
    what x = " A LONG LIST."
