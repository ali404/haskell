-- INF 1 Functional Programming
--
-- Indexed data represented as a tree


module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList,
                    merge, filterLT, filterGT
                  )

where

-- Modules for testing

import Test.QuickCheck
import Control.Monad
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf
                               (Node 5 40 Leaf Leaf ))

-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depthHelper :: Ord k => Int -> Keymap k a -> [Int]
depthHelper n Leaf = [n]
depthHelper n (Node _ _ left right) = depthHelper (n+1) left ++ depthHelper (n+1) right

depth :: Ord k => Keymap k a -> Int
depth tree = maximum $ depthHelper 0 tree

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node x y left right) = toList left ++ [(x,y)] ++ toList right

-- Exercise 8

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = f
    where
      f Leaf = Node key value Leaf Leaf
      f (Node k v left right) | key == k  = Node k value left right
                              | key <= k  = Node k v (f left) right
                              | otherwise = Node k v left (f right)

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get searchedValue Leaf = Nothing
get searchedValue (Node k val left right)
    | searchedValue == k = Just val
    | searchedValue <= k = get searchedValue left
    | otherwise = get searchedValue right

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10

fromListHelper :: Ord k => [(k,a)] -> Keymap k a -> Keymap k a
fromListHelper (pair:pairs) tree = fromListHelper pairs $ set (fst pair) (snd pair) tree
fromListHelper [] tree = tree

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList pairs = fromListHelper pairs Leaf


prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where
      zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where
      zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 12

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT highVal (Node k val left right)
    | highVal <= k = filterLT highVal left
    | highVal > k  = (Node k val left (filterLT highVal right))
filterLT _ Leaf = Leaf

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT lowVal (Node k val left right)
    | lowVal >= k = filterGT lowVal right
    | lowVal < k  = (Node k val (filterGT lowVal left) right)
filterGT _ Leaf = Leaf

-- Exercise 13

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge firstTree secondTree = fromList $ toList firstTree ++ toList secondTree

prop_merge :: (Ord k, Eq a) => Keymap k a -> Keymap k a -> Bool
prop_merge fT sT = firstPass && secondPass && thirdPass
    where
        firstPass = merge fT fT == fT && merge sT sT == sT
        secondPass = merge fT Leaf == fT && merge sT Leaf == sT
        thirdPass = merge fT sT == merge sT fT

-- Exercise 14

del :: Ord k => k -> Keymap k a -> Keymap k a
del _ Leaf = Leaf
del deletedKey (Node k v left right)
    | deletedKey < k = (Node k v (del deletedKey left) right)
    | deletedKey > k = (Node k v left (del deletedKey right))
    | otherwise = merge left right

-- Exercise 15

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select restriction tree = fromList $ selectToList restriction tree
    where
        selectToList :: Ord k => (a -> Bool) -> Keymap k a -> [(k, a)]
        selectToList restriction tree = filter (\(x,y) -> restriction y) (toList tree)

-- Instances for QuickCheck -----------------------------
instance (Ord k, Show k, Show a) => Show (Keymap k a) where
    show = show . toList

instance (Ord k, Eq a) => Eq (Keymap k a) where
    (==) a b = toList a == toList b

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary
