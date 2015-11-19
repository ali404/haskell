-- Informatics 1 Functional Programming
-- Tutorial 6
--
-- Due: 12/13 November

import System.Random
import Data.List


-- Importing the keymap module

import KeymapList


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]
 

-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen pairs = maximum $ map (\x -> length $ fst $ snd x ) pairs

formatLine :: Int -> (Barcode, Item) -> String
formatLine maxLen pair = barcode ++ replicate (maxLen - barcodeLen + 1) '.' ++ product ++ replicate (maxLen - productLen + 1) '.' ++ unit 
                       where barcode = fst pair
                             product = fst $ snd pair
                             unit = snd $ snd pair
                             barcodeLen = length barcode
                             productLen = length product

showCatalogue :: Catalogue -> String
showCatalogue catalogue =foldl (\x y -> x++"\n"++y) "" $ map (formatLine maxLen) catalogueList
                          where catalogueList = toList catalogue
                                maxLen = longestProductLen catalogueList

-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList maybeEntry 
   | maybeEntry == Nothing = []
   | otherwise = [Just maybeEntry]

listToMaybe :: (Eq a) => [a] -> Maybe a
listToMaybe list
   | list == [] = Nothing
   | otherwise = Just (head list)

catMaybes :: [Maybe a] -> [a]
catMaybes = undefined

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems = undefined






-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
