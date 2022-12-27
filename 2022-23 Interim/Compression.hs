module Compression where

import Data.List
import Data.Char

data HTree a = Leaf Int a | Node Int (HTree a) (HTree a)
               deriving (Show)

instance Eq (HTree a) where
  t1 == t2 = freqCount t1 == freqCount t2

instance Ord (HTree a) where
  t1 <= t2' = freqCount t1 <= freqCount t2'

type Code = [Int]

freqCount :: HTree a -> Int
freqCount (Leaf n a)
  = n
freqCount (Node n t1 t2)
  = n

testString :: String
testString
  = "mississippi is missing"

--
-- Example Huffman coding tree from the spec.
--
fig :: HTree Char
fig
  = Node 22 (Node 8 (Node 4 (Leaf 2 'p') (Leaf 2 ' '))
                    (Node 4 (Node 2 (Leaf 1 'n') (Leaf 1 'g'))
                            (Leaf 2 'm')))
            (Node 14 (Leaf 7 'i') (Leaf 7 's'))

----------------------------------------------------------------------------

count :: Eq a => a -> [a] -> Int
count x list = length (filter (==x) list)
{-count _ [] = 0
count obj list@(x:xs)
    | obj == x  = 1 + count obj xs
    | otherwise = count obj xs
-}

countAll :: Eq a => [a] -> [a] -> [(a, Int)]
countAll s1 s2 = map (\x -> (x, count x s2)) s1

buildTable :: Eq a => [a] -> [(a, Int)]
-- Nub function already used in countall to remove duplicates 
--(if I was not supposed to use nub in countAll, I would simply remove it there and change build table to nub (countAll s s))
buildTable s = countAll (nub s) s

merge :: HTree a -> HTree a -> HTree a
--Pre: the left tree always has a lower frequency count
merge t1 t2 = Node totalFreq t1 t2
    where totalFreq = freqCount t1 + freqCount t2

reduce :: [HTree a] -> HTree a
-- Pre: The argument list non-empty and sorted based on the ordering function
--      in the Ord instance above.
reduce [t] = t
reduce (t1: t2: ts) = reduce (insert newTree ts)
    where newTree = merge t1 t2
--alternatively: reduce (t1:ts) = foldr (merge) t1 ts?

buildTree :: Eq a => [a] -> HTree a
-- Pre: The list is non-empty
buildTree items = reduce leaves
    where
        itemCounts = buildTable items
        leaves = [(Leaf num i) | (i, num) <- itemCounts]

encode :: Eq a => [a] -> HTree a -> Code
-- Pre: The tree can encode each of the items the list
encode [] _         = []
encode _ (Leaf _ _) = [] --inCase huffTree only has one item in it
encode items@(i:is) huffTree 
    = checkItemInTree i huffTree ++ encode is huffTree
        where
            checkItemInTree :: Eq a => a -> HTree a -> [Int]
            checkItemInTree item (Leaf _ item')
                | item == item' = []
                | otherwise     = [-1]
            checkItemInTree item (Node _ lt@(Leaf _ i) rt)
                | item == i = [0] 
                | otherwise = [1] ++ checkItemInTree item rt
            checkItemInTree item (Node _ lt rt)
                | (-1) `elem` checkItemInTree item lt = [1] ++ checkItemInTree item rt
                | otherwise = [0] ++ checkItemInTree item lt


decode :: Code -> HTree a -> [a]
-- Pre: The code is valid with respect to the tree
decode code huffTree = decode' code huffTree 
      where
        decode' code' (Leaf _ item)
            | code' == [] = [item]  
            |otherwise    = [item] ++ decode' code' huffTree
        decode' code'@(n:ns) (Node _ lt rt)
            | n == 0 = decode' ns lt
            | n == 1 = decode' ns rt

compressTree :: HTree Char -> [Int]
compressTree
  = undefined

rebuildTree :: [Int] -> HTree Char
-- Pre: The bitstring ([Int]) is a valid encoding of a Huffman tree
--      of characters
rebuildTree
  = undefined

-----------------------------------------------------------------------------
-- THE SECRET TEST STRING (GIVEN)...
--

secretString :: String
secretString
  = decode code (rebuildTree tree)
  where
    code = [1,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,
            1,1,1,1,1,1,1,1,1,1,0,1,0,1,1,1,0,0,1,1,1,0,1,1,0,1,1,
            0,1,0,0,1,0,1,1,1,1,1,0,1,0,1,1,0,0,0,1,1,0,1,0,0,0,1,
            1,0,0,0,1,0,1,1,0,0,1,1,0,1,1,0,1,0,0,1,1,1,0,1,0,0,0,
            1,0,1,0,0,0,1,1,0,1,0]
    tree = [0,0,0,0,1,1,1,1,1,0,0,0,1,1,1,0,0,1,0,1,0,1,1,1,0,0,0,
            1,1,1,1,1,1,0,1,1,1,0,0,1,1,1,0,1,1,1,1,1,1,1,0,1,1,1,
            0,0,1,1,1,0,1,1,0,0,1,1,1,0,0,1,0,0,0,0,1,1,1,0,1,0,1,
            1,0,1,0,1,0,0,0,0,1,0,1,1,1,1,0,0,1,0,1,1,1,1,0,0,1,1,
            0,0,0,1,0,1,0,1,1,0,1,1,0,1,0,1,1,0,0,0,1,0,1,0,0,0,0,
            0,1,0,1,0,1,0,1,1,0,0,1,1,1,0,1,1,0,1,1,1,1,0,0,0,0,1,
            0,1,0,1,1,0,0,0,1,1,0,1,1,0,0,0,0]
