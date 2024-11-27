import Data.Char
import Data.List
import Data.Map qualified as Map

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

encode :: Int -> String -> String
encode offset = map (chr . (+ offset) . ord)

decode :: Int -> String -> String
decode offset = encode (negate offset)

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1 ..]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k, v) : xs)
  | key == k = Just v
  | otherwise = findKey key xs

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs

phoneBook :: Map.Map String String
phoneBook =
  Map.fromList
    [ ("burt", "123"),
      ("mr. bingus", "456"),
      ("tubs", "789"),
      ("rod", "354")
    ]

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs