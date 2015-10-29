-- Informatics 1 - Functional Programming
-- Tutorial 4
--
-- Due: the tutorial of week 6 (22/23 Oct)

import Data.List (nub, transpose)
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

(|>) :: a -> (a -> b) -> b
a |> b = b a

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

testURL     = "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/testpage.html"

testHTML :: String
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br><b>TA:</b> "
            , "mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Don Sannella","dts@inf.ed.ac.uk")
               , ("Karoliina Lehtinen","m.k.lehtinen@sms.ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

-- 1.
isSmallDigit :: Char -> Bool
isSmallDigit c1
    | ord c1 >= 97 && ord c1 <= 122 = True
    | otherwise = False

isBigDigit :: Char -> Bool
isBigDigit c1
    | ord c1 >= 65 && ord c1 <= 90 = True
    | otherwise = False

sameChar :: Char -> Char -> Bool
sameChar  c1 c2
    | isSmallDigit c1 && isBigDigit c2 = c1 == chr (ord c2 + 32)
    | isBigDigit c1 && isSmallDigit c2 = chr (ord c1 + 32) == c2
    | otherwise = c1 == c2

sameString :: String -> String -> Bool
sameString s1 s2
    | length s1 /= length s2 = False
    | otherwise = foldl (&&) True $ map (\x -> sameChar (fst x)  (snd x)) ss
                where ss = zip s1 s2
-- 2.
prefix :: String -> String -> Bool
prefix c1 c2 = sameString c1 $ take (length c1) c2


--WTF is this?! apparently they use toLower and toUpper
--to chars, when in the pdf is stated that these functions
--have weird outputs for weird inputs (like miu, or \237)...OMG
prop_prefix_pos :: String -> Int -> Bool
prop_prefix_pos str n =  prefix substr (map toLower str) &&
		         prefix substr (map toUpper str)
                           where
                             substr  =  take n str

prop_prefix_neg :: String -> Int -> Bool
prop_prefix_neg str n = sameString str substr || (not $ prefix str substr)
                          where substr = take n str
--this quickCheck is stupid

-- 3.
contains :: String -> String -> Bool
contains c1 c2 = foldl (||) False
                        $ map (\x -> prefix c2 $ drop x c1) [0..length c1]

prop_contains :: String -> Int -> Int -> Bool
prop_contains = undefined


-- 4.
getFirstOccurance' :: String -> String -> Int
getFirstOccurance' _ [] = 0
getFirstOccurance' c1 c2
    | prefix c1 c2 = length c2
    | otherwise = getFirstOccurance' c1 (tail c2)

getFirstOccurance :: String -> String -> Int
getFirstOccurance c1 c2 = length c2 - getFirstOccurance' c1 c2

takeUntil :: String -> String -> String
takeUntil c1 c2 = take (getFirstOccurance c1 c2) c2

dropUntil :: String -> String -> String
dropUntil c1 c2 = drop ((getFirstOccurance c1 c2) + length c1) c2

-- 5.
split :: String -> String -> [String]
split [] _ = error "separator must be at least length one"
split _ [] = [""]
split separator fullString
    | contains fullString separator = [takeUntil separator fullString] ++ split separator (dropUntil separator fullString)
    | otherwise = [fullString]



reconstruct' :: String -> [String] -> String
reconstruct' [] _ = error "separator must be at least length one"
reconstruct' _ [] = ""
reconstruct' _ [""] = ""
reconstruct' separator (x:xs) = x ++ separator ++ reconstruct separator xs

reconstruct :: String -> [String] -> String
reconstruct separator splittedString
    | length splittedString == 1 = foldl (++) "" splittedString
    | otherwise = reconstruct' separator splittedString

prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML html = split "<a href=\"" $ dropUntil "<a href=\"" html

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails [] = []
takeEmails (x:xs)
    | prefix "mailto" x = [x] ++ takeEmails xs
    | otherwise = takeEmails xs


-- 8.
link2pair :: Link -> (Name, Email)
link2pair link = (takeUntil "</a>" $ dropUntil "\">" link, dropUntil "mailto:" $ takeUntil "\">" link)


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML html = html
                    |> linksFromHTML
                    |> takeEmails
                    |> map link2pair
                    |> nub

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail _ [] = []
findEmail requiredName (first:list)
    | contains (fst first) requiredName = [first]
    | otherwise = findEmail requiredName list


-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML html requiredName =
            html
            |> emailsFromHTML
            |> findEmail requiredName


-- Optional Material

-- 12.
hasInitials :: String -> Name -> Bool
hasInitials initials name = sameString initials $ head $ transpose $ split " " name

-- 13.
emailsByMatchFromHTML :: (Name -> Bool) -> HTML -> [(Name, Email)]
emailsByMatchFromHTML restriction html =
                html
                |> emailsFromHTML
                |> filter (\x -> restriction $ fst x)

emailsByInitialsFromHTML :: String -> HTML -> [(Name, Email)]
emailsByInitialsFromHTML initials html =  emailsByMatchFromHTML (hasInitials initials) html

-- 14.

-- If your criteria use parameters (like hasInitials), change the type signature.
myCriteria :: String -> Name -> Bool
myCriteria _ [] = True
myCriteria [] _ = False
myCriteria (first:fullName) initials
    | sameChar first $ head initials = myCriteria fullName $ tail initials
    | otherwise = myCriteria fullName initials

emailsByMyCriteriaFromHTML :: HTML -> [(Name, Email)]
emailsByMyCriteriaFromHTML html = emailsByMatchFromHTML (myCriteria "DS") html

-- 15
splitName :: String -> String
splitName s = foldl (++) "" (tail splittedString) ++ ", " ++ head splittedString
    where splittedString = split " " s

findLongestString :: [(String, String)] -> Int
findLongestString xs = foldl (\curr x -> if(curr > x) then curr else x ) 0 $ map (\x -> length $ fst x) xs

ppAddrBook :: [(Name, Email)] -> String
ppAddrBook emails = unlines $ map (\email -> (splitName $ fst email) ++ replicate (highestLength - (length $ fst email) + 5) ' ' ++ snd email) emails
    where highestLength = findLongestString emails
