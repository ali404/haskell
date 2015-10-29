-- Informatics 1 - Functional Programming
-- Tutorial 2
--
-- Week 4 - due: 15/16 Oct.

import Data.Char
import Data.List
import Test.QuickCheck


-- 1.
rotate :: Int -> [Char] -> [Char]
rotate shiftCount rawText
	| shiftCount < 0 || shiftCount > length rawText = error "err"
	| otherwise = drop shiftCount rawText ++ take shiftCount rawText

-- 2.
-- verifies if by rotating twice the string (first with m, then with 1-m)
-- it gives the same result (initial str)
-- it resolves the error by reducing to 0 or to k mod l
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3.
makeKey :: Int -> [(Char, Char)]
makeKey shiftCount = zip unshiftedAlphabet shiftedAlphabet
						where
							unshiftedAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
							shiftedAlphabet = rotate shiftCount unshiftedAlphabet

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp undecipheredKey lookupList
	| length matches == 1 = matches!!0
	| otherwise = undecipheredKey
	where
		matches = [snd keyTuple | keyTuple <- lookupList, fst keyTuple == undecipheredKey]

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec undecipheredKey [] = undecipheredKey
lookUpRec undecipheredKey (x:xs)
	| fst x == undecipheredKey = snd x
	| otherwise = lookUpRec undecipheredKey xs


prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp a b = lookUp a b == lookUpRec a b

-- 5.
encipher :: Int -> Char -> Char
encipher shiftCount undecipheredKey = lookUp undecipheredKey (makeKey shiftCount)

-- 6.
normalize :: String -> String
normalize [] = []
normalize (currChar:restChars)
	| isDigit currChar || isAlpha currChar = (toUpper currChar):(normalize restChars)
	| otherwise = normalize restChars

-- 7.
encipherStr :: Int -> String -> String
encipherStr cipherCount undecipheredText = [ encipher cipherCount a | a<-normalisedText ]
	where
		normalisedText = normalize undecipheredText

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey cipherKey = [(snd key, fst key) | key <- cipherKey]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec (key:cipherKey) = [(snd key, fst key)] ++ reverseKeyRec cipherKey

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey a = reverseKey a == reverseKeyRec a

-- 9.
decipher :: Int -> Char -> Char
decipher shiftCount undecipheredChar = lookUp undecipheredChar (reverseKey (makeKey shiftCount))

decipherStr :: Int -> String -> String
decipherStr shiftCount [] = []
decipherStr shiftCount (x:xs)
	| x == ' ' || isDigit x || 65 <= ord x && 90 >= ord x = (decipher shiftCount x):(decipherStr shiftCount xs)
	| otherwise = decipherStr shiftCount xs

-- 10.
contains :: String -> String -> Bool
contains s1 s2 =
    let numbers :: [Int]
        numbers = [0..(length s1 - length s2)]
        pos :: [String]
        pos = [take (length s2) (drop a s1) | a <- numbers]
        finds :: [String]
        finds = [ x | x<- pos, x == s2 ]
    in if length finds >= 1 then True else False

-- 11.
candidates :: String -> [(Int, String)]
candidates cipher = [ (shiftCount, (decipherStr shiftCount cipher)) | shiftCount <- [1..26], contains (decipherStr shiftCount cipher) "THE" || contains (decipherStr shiftCount cipher) "AND" ]



-- Optional Material

-- 12.
splitEachFive :: String -> [String]
splitEachFive unsplittedString = [ splittedString | x <- [1..((div (length unsplittedString - 1) 5)+1)], let split = take 5 (drop (5*(x-1)) unsplittedString), let splittedString = split ++ replicate (5 - length split) '\0' ]

-- 13.
prop_transpose :: String -> Bool
prop_transpose a = splitEachFive a == transpose (transpose (splitEachFive a))

-- 14.
encrypt :: Int -> String -> String
addTogether :: [String] -> String
addTogether [] = ""
addTogether (x:xs) = x ++ addTogether xs

encrypt cypher encryptString = addTogether (transpose (splitEachFive (encipherStr cypher encryptString)))

-- 15.
decrypt :: Int -> String -> String
--separateEachFive :: String -> [String]

decrypt cypher decryptString = decipherStr cypher (addTogether (transpose (splitEachFive decryptString)))

-- Challenge (Optional)

-- 16.
addToList :: Char -> [(Char, Int)] -> [(Char, Int)]
addToList char [] = [(char, 1)]
addToList char (elem:list)
	| char == fst elem = [(char, snd elem + 1)] ++ list
	| otherwise = [elem] ++ addToList char list

countFreqsWithList :: String -> [(Char, Int)] -> [(Char, Int)]
countFreqsWithList [] list = list
countFreqsWithList (x:xs) list = countFreqsWithList xs (addToList x list)

countFreqs :: String -> [(Char, Int)]
countFreqs s = countFreqsWithList s []

-- 17
--freqDecipher :: String -> [String]
--freqDecipher = undefined
