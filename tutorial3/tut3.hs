-- Informatics 1 - Functional Programming
-- Tutorial 3
--
-- Week 5 - Due: 22/23 Oct.

import Data.Char
import Data.List
import Test.QuickCheck

(|>) :: a -> (a -> b) -> b
a |> b = b a

-- 1. Map
-- a.
uppers :: String -> String
uppers lowercaseInput = map toUpper lowercaseInput

-- b.
doubles :: [Int] -> [Int]
doubles ints = map (*2) ints

-- c.
penceToPounds :: [Int] -> [Float]
penceToPounds prices = map (\x -> fromIntegral x/100) prices

-- d.
uppers' :: String -> String
uppers' lowercaseInput = [toUpper loweredLetter | loweredLetter <- lowercaseInput]

prop_uppers :: String -> Bool
prop_uppers xs = uppers' xs == uppers xs



-- 2. Filter
-- a.
alphas :: String -> String
alphas inputString = filter isAlpha inputString

-- b.
rmChar ::  Char -> String -> String
rmChar removedChar inputString = filter (\x -> x /= removedChar) inputString

-- c.
above :: Int -> [Int] -> [Int]
above maxValue ints = filter (\x -> x < maxValue) ints

-- d.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals intPairs = filter(\x -> fst x /= snd x) intPairs

-- e.
rmCharComp :: Char -> String -> String
rmCharComp removedChar inputString = [remainedChar | remainedChar <- inputString, remainedChar /= removedChar]

prop_rmChar :: Char -> String -> Bool
prop_rmChar a b = rmChar a b == rmCharComp a b



-- 3. Comprehensions vs. map & filter
-- a.
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

upperChars' :: String -> String
upperChars' s =
    s
        |> filter isAlpha
        |> map toUpper

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' xs = xs
                        |> filter (>3)
                        |> map (*2)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' strs = strs
                        |> filter (\x -> even (length x))
                        |> map reverse

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- apparently i am using FOLDL, because i am dangerous , rawr!
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold xs = foldl (*) 1 xs

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) = 
    | x == False = False
    | otherwise = andRec xs

andFold :: [Bool] -> Bool
andFold xs = foldl (&&) True xs

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs

-- c.
concatRec :: [[a]] -> [a]
concatRec [[]] = []
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold xs = foldl (++) [] xs

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.
rmCharsRec :: String -> String -> String
rmCharsRec [] _  = []
rmCharsRec (x:xs) ss = rmCharsRec xs (rmChar x ss)

rmCharsFold :: String -> String -> String
rmCharsFold xs ss = foldr (rmChar) ss xs

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str



type Matrix = [[Int]]


-- 5
-- a.
uniform :: [Int] -> Bool
uniform matrix =
    matrix
        |> map (\x -> fromIntegral x ==  (fromIntegral $ sum matrix)/ (fromIntegral $ length matrix))
        |> foldl (&&) True

-- b.
valid :: Matrix -> Bool
valid matrix = matrix
    |> map length
    |> if (matrix /= [[]] && length (matrix!!0) >= 1) then uniform else (\x -> False)

-- 6.

-- a : 18
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [f (fst z) (snd z) | z <-zs]
                        where zs = zip xs ys

zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys =
    zip xs ys
    |> map (\x -> uncurry f x)

prop_zip :: (Eq c) => (a -> b -> c) -> [a] -> [b] -> Bool
prop_zip a b c = (zipWith a b c == zipWith' a b c) && (zipWith'' a b c == zipWith a b c)

-- 7.
plusM :: Matrix -> Matrix -> Matrix
plusM m1 m2  =
    | valid m1 && valid m2 && length m1!!0 == length m2!!0 && length m1 == length m2 = zipWith (\x y -> zipWith (+) x y) m1 m2
    | otherwise = error "Incompatible Matrices"

-- 8.
timesM' :: Matrix -> Matrix -> Matrix
timesM' m1 m2 = map (\x -> map (\y -> sum (zipWith (*) x y )) m2 ) m1


timesM :: Matrix -> Matrix -> Matrix
timesM m1 m2 =
    | valid m1 && valid m2 && length m1!!0 == length m2 = timesM' m1 (transpose m2)
    | otherwise = error "Incompatible Matrices"

-- Optional material
-- 9.
