type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I
value :: BinTree a -> a
value (Node v _ _) = v

rank :: BinTree a -> Int
rank (Node _ r _) = r

children :: BinTree a -> [BinTree a]
children (Node _ _ cs) = cs

combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
--Pre: Both trees have the same rank (according to section 2.1)
combineTrees t1 t2
    | value t1 < value t2 = Node (value t1) newRank ([t2] ++ (children t1))
    | otherwise           = Node (value t2) newRank ([t1] ++ (children t2))
        where newRank = (rank t1) + 1

--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
--Pre: Heap is non-empty
extractMin trees = minimum [value tree | tree <- trees]

mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps [] h2 = h2
mergeHeaps h1 [] = h1
mergeHeaps h1@(t:ts) h2@(t':ts')
    | rank t < rank t' = [t] ++ mergeHeaps ts h2
    | rank t > rank t' = [t'] ++ mergeHeaps h1 ts'
    | otherwise        = mergeHeaps ts ([combineTrees t t'] ++ ts' )

insert :: Ord a => a -> BinHeap a -> BinHeap a
insert val heap = mergeHeaps [Node val 0 []] heap

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin [] = []
deleteMin h = newHeap h' subTrees
    where 
        (minimumHeap, h')            = removeMin h 
        subTrees      = reverse (children minimumHeap)
        newHeap :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
        newHeap heap []  = heap
        newHeap [] sts = sts
        newHeap heap@(t:ts) subts@(t':ts')
            | rank t == rank t' = newHeap (mergeHeaps ts [combineTrees t t']) ts'
            | otherwise         = heap ++ subts 


remove :: Eq a => a -> BinHeap a -> BinHeap a
remove min (t:ts)
    | value t == min = ts
    | otherwise      = [t] ++ remove min ts

removeMin :: Ord a => BinHeap a -> (BinTree a, BinHeap a)
removeMin heap = (min, remove (value min) heap)
    where min = head (filter (\x -> value x == extractMin heap) heap)

binSort :: Ord a => [a] -> [a]
binSort []       = []
binSort elements = extract heaps
    where 
        extract [] = []
        extract heaps'
            = [extractMin heaps'] ++ extract (deleteMin heaps')
        createHeap (e:es) h 
            | es == []  = insert e h
            | otherwise = createHeap es (insert e h)
        heaps = createHeap elements []


--------------------------------------------------------------
-- PART III

toBinary :: BinHeap a -> [Int]
toBinary [] = [0]
toBinary heap = storeResult heap [0]
    where
        storeResult :: BinHeap a -> [Int] -> [Int]
        storeResult [] num = num
        storeResult heap'@(t@(Node _ _ []):ts) binaryList 
            = storeResult ts (addOne binaryList)
        storeResult heap'@(t@(Node _ _ h):ts) binaryList
            = storeResult ts (storeResult h (addOne binaryList))

addOne :: [Int] -> [Int]
addOne [] = [1]
addOne num
    | last num == 0 = init num ++ [1]
    | otherwise     = addOne (init num) ++ [0]

{- FOLLOWING FUNCTION ONLY USED TO HELP ME THINK ABOUT THE ANSWER 
   AND VERIFY MY PART 3 SOLUTIONS TO SOME EXTENT
findNumNodes :: BinHeap a -> Int
findNumNodes [] = 0
findNumNodes heap@(t@(Node _ _ []):ts) = 1 + findNumNodes ts
findNumNodes heap@(t@(Node _ _ h):ts) 
    = 1 + findNumNodes h + findNumNodes ts
-}

binarySum :: [Int] -> [Int] -> [Int]
binarySum [] [] = [0]
binarySum n1 [] = n1
binarySum [] n2 = n2
binarySum n1 n2
    | last n2 == 1 = binarySum (init n1') (init n2) ++ [last n1']
    | otherwise    = binarySum (init n1) (init n2) ++ [last n1]
      where
        n1' = addOne n1

------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]


