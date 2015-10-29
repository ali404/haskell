import Data.Char
import Test.QuickCheck
import Data.List

-----------------
--exercise 1
-----------------

halveEvens :: [Int] -> [Int]
halveEvens xs =  [ div a 2 | a <- xs, mod a 2 == 0]

halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs)
    | mod x 2 == 1 = halveEvensRec xs
    | mod x 2 == 0 = (div x 2) : halveEvensRec xs

prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensRec xs

-----------------
--exercise 2
-----------------

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b xs
    | a > b = []
    | otherwise = [x | x <- xs, x >= a, x <= b]

inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec _ _ [] = []
inRangeRec a b _
    | a > b = []
inRangeRec a b (x:xs)
    | x >= a && x <= b = x : inRangeRec a b xs
    | otherwise = inRangeRec a b xs

prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange a b xs = inRange a b xs == inRangeRec a b xs

-----------------
-- exercise 3
-----------------

countPositives :: [Int] -> Int
countPositives [] = 0
countPositives xs = sum [1 | a <- xs , a > 0]

countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs)
    | x > 0 = 1 + countPositivesRec xs
    | otherwise = countPositivesRec xs

prop_countPositives :: [Int] -> Bool
prop_countPositives xs = countPositives xs == countPositivesRec xs

-----------------
-- exercise 4
-----------------

discount :: Int -> Int -> Int
discount a b
    | b == 0 = a
    | b > 100 = error "discount rate can't be higher then 100"
    | otherwise =
        let x = fromIntegral a
            y = fromIntegral b
        in round (x - x * y / 100)

pennypincher :: [Int] -> Int
pennypincher [] = 0
pennypincher xs =  sum [discount a 10 | a <- xs, a > 0, discount a 10 <= 19900]

pennypincherRec :: [Int] -> Int
pennypincherRec [] = 0
pennypincherRec (x:xs)
    | discount x 10 <= 19900 && x > 0 = discount x 10 + pennypincherRec xs
    | otherwise  = pennypincherRec xs

prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs == pennypincherRec xs


-----------------
--exercise 5
-----------------

multDigits :: String -> Int
multDigits xs = product [digitToInt a | a <- xs, isDigit a]

multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs)
	| isDigit x = digitToInt x * multDigitsRec xs
	| otherwise = multDigitsRec xs
	
prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs == multDigitsRec xs

-----------------
--exercise 6
-----------------

capitalise :: String -> String
capitalise [] = []
capitalise (x:xs) = [toUpper x] ++ [toLower a | a <- xs]

make'emLow :: String -> String
make'emLow [] = []
make'emLow (x:xs) = [toLower x] ++ make'emLow xs

capitaliseRec :: String -> String
capitaliseRec [] = []
capitaliseRec (x:xs) = [toUpper x] ++ make'emLow xs

-- only recursive
--capRec :: String -> String
--capRec [] = []
--capRec (xs:x)
--	| length xs == 0 = [toUpper x]
--	| otherwise = capRec xs ++ [toLower x]
-- without helper

prop_capitalise :: String -> Bool
prop_capitalise x = capitalise x == capitaliseRec x

-----------------
--exercise 7
-----------------

decap :: String -> String
decap [] = []
decap xs = [toLower a | a <- xs]

title :: [String] -> [String]
title [] = []
title (x:xs) = capitalise x : [ if length a < 4 then decap a else capitalise a | a <- xs ]

titleRec  :: [String] -> [String]
titleRec [] = []
titleRec (xs)
	| length (last xs) == 1 = [capitalise (last xs)]
	| length (last xs) < 4 = titleRec (init xs)++[decap (last xs)]
    | otherwise = titleRec (init xs)++[capitalise (last xs)]

prop_title :: [String] -> Bool
prop_title xs = title xs == titleRec xs

-----------------
--exercise 8
-----------------

crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind _ _ _ [] = []
crosswordFind c a b xs = [d | d <- xs, length d == b , a > 0, a < b, d!!a == c]

crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec _ _ _ [] = []
crosswordFindRec c a b (x:xs)
	| length x < a || a < 0 = crosswordFindRec c a b xs
    | length x == b && a > (-1) && a<b && x!!a == c = x : crosswordFindRec c a b xs
    | otherwise = crosswordFindRec c a b xs

prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind a b c d = crosswordFind a b c d  == crosswordFindRec a b c d

-----------------
--exercise 9
-----------------

search :: String -> Char -> [Int]
search xs a =
    let pos :: [(Char, Int)]
        pos = zip xs [0..]
    in [ snd x | x <- pos, fst x == a ]

searchRec :: String -> Char -> [Int]
searchWithPos :: String -> Char -> Int -> [Int]
searchWithPos [] _ _ = []
searchWithPos (x:xs) a pos
	| x == a = pos:searchWithPos xs a (pos+1)
	| otherwise = searchWithPos xs a (pos+1)
searchRec xs a = searchWithPos xs a 0

prop_search :: String -> Char -> Bool
prop_search a b = search a b == searchRec a b

-----------------
--exercise 10
-----------------

contains :: String -> String -> Bool
contains s1 s2 =
    let numbers :: [Int]
        numbers = [0..(length s1 - length s2)]
        pos :: [String]
        pos = [take (length s2) (drop a s1) | a <- numbers]
        finds :: [String]
        finds = [ x | x<- pos, x == s2 ]
    in if length finds >= 1 then True else False

containsRec :: String -> String -> Bool
containsRec "" "" = True
containsRec "" _ = False
containsRec _ "" = True
containsRec (x:xs) s2
	| length xs == 0 && length s2 == 1 && x == head s2 = True
    | take (length s2) xs == s2 = True
    | otherwise = containsRec xs s2

prop_contains :: String -> String -> Bool
prop_contains a b = contains a b == containsRec a b
