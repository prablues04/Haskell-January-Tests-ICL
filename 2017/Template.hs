import Data.Maybe
import Data.List

type AttName = String

type AttValue = String

type Attribute = (AttName, [AttValue])

type Header = [Attribute]

type Row = [AttValue]

type DataSet = (Header, [Row])

data DecisionTree = Null |
                    Leaf AttValue | 
                    Node AttName [(AttValue, DecisionTree)]
                  deriving (Eq, Show)

type Partition = [(AttValue, DataSet)]

type AttSelector = DataSet -> Attribute -> Attribute

xlogx :: Double -> Double
xlogx p
  | p <= 1e-100 = 0.0
  | otherwise   = p * log2 p 
  where
    log2 x = log x / log 2

lookUp :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
lookUp x table
  = fromMaybe (error ("lookUp error - no binding for " ++ show x ++ 
                      " in table: " ++ show table))
              (lookup x table)

--------------------------------------------------------------------
-- PART I
--------------------------------------------------------------------

allSame :: Eq a => [a] -> Bool
allSame []     = True
allSame (x:[]) = True
allSame (x1:x2:xs)
    | x1 == x2  = allSame (x2:xs)
    | otherwise = False

remove :: Eq a => a -> [(a, b)] -> [(a, b)]
--Removing all tuples with first value in pair as x -> specification is not clear whether to remove all instances of only first instance
remove x list = filter (\element -> fst element /= x) list

lookUpAtt :: AttName -> Header -> Row -> AttValue
--Pre: The attribute name is present in the given header.
lookUpAtt _ [] _ = [] --Base case is in fact not needed given our pre condition
lookUpAtt name header@(c@(cname,_):cs) row@(x:xs)
    | name == cname = x
    | otherwise     = lookUpAtt name cs xs

removeAtt :: AttName -> Header -> Row -> Row
removeAtt name header row = delete (lookUpAtt name header row) row
--removeAtt _ _ [] = []
{-
removeAtt name header@(c@(cname,_):cs) row@(x:xs)
    | name == cname = xs
    | otherwise     = x:removeAtt name cs xs
-}
-- removeAtt name header row = map (snd) (remove name (zip (map (fst) header) row))

addToMapping :: Eq a => (a, b) -> [(a, [b])] -> [(a, [b])]
-- Pre: there exists at most one binding for the first value in the tuple within the mapping
addToMapping (x, v) [] = [(x, [v])]
addToMapping (x, v) (pairs@(p@(x',vs):ps))
    | x == x'   = (x,(v:vs)):ps
    | otherwise = p:addToMapping (x, v) ps

buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
-- Pre: Each row of the data set contains an instance of the attribute
buildFrequencyTable (cname, attValues) (header,rows)
    = map (\c -> (c, count values c)) attValues
      where 
        values
            | rows == [] = []
            | otherwise  = map (lookUpAtt cname header) rows
        count :: [AttValue] -> AttValue -> Int
        count [] _ = 0
        count (v:vs) condition
            | v == condition = 1 + count vs condition
            | otherwise     = count vs condition

--------------------------------------------------------------------
-- PART II
--------------------------------------------------------------------

nodes :: DecisionTree -> Int
nodes Null = 0
nodes (Leaf _) = 1
nodes (Node _ connections)
    = foldr (+) 1 subTreeSizes
      where 
        subTreeSizes = map (\(attVal, subTree) -> nodes subTree) connections

evalTree :: DecisionTree -> Header -> Row -> AttValue
evalTree (Leaf x) _ _ = x
evalTree (Null) _ _   = ""
evalTree (Node attr connections) header row
    = evalTree (lookUp condition connections) header row
    where
        condition = lookUpAtt attr header row

--------------------------------------------------------------------
-- PART III
--------------------------------------------------------------------

--
-- Given...
-- In this simple case, the attribute selected is the first input attribute 
-- in the header. Note that the classifier attribute may appear in any column,
-- so we must exclude it as a candidate.
--
nextAtt :: AttSelector
--Pre: The header contains at least one input attribute
nextAtt (header, _) (classifierName, _)
  = head (filter ((/= classifierName) . fst) header)

partitionData :: DataSet -> Attribute -> Partition
partitionData dataset@(headr,rows) attr = map (\(attVal, rows') -> (attVal, (newHeader, rows'))) partitions 
    where
        segregate :: Attribute -> [Row] -> [(AttValue, [Row])] -> [(AttValue, [Row])]
        segregate (attName, values@(_:vs)) [] mapping
            = segregate (attName, vs) rows mapping
        segregate (_,[]) _ mapping = map (\(x,v) -> (x, reverse v)) mapping
        segregate attr'@(attName, values@(v:_)) (r:rs) mapping
            | v == valInRow = segregate attr' rs (addToMapping (v, r') mapping)
            | otherwise     = segregate attr' rs mapping
              where
                r' = delete v r
                valInRow = lookUpAtt attName headr r
        partitions = segregate attr rows []
        newHeader = remove (fst attr) headr
        {-
        segregate :: Attribute -> [Row] -> [[Row]]
        segregate (attName, values@(_:vs)) [] 
            = segregate (attName, vs) rows
        segregate (_, []) _ = []
        segregate attr'@(name, values@(v:_)) rows'@(r:rs)
            | v == attValInRow = [[r]] ++ segregate attr' rs
            | otherwise        = segregate attr' rs
            where attValInRow = lookUpAtt name h r
        segregatedVals = map (\r' -> map (removeAtt (fst attr) h) r') (segregate attr rows)
        h' = removeAtt (fst attr) h (head (snd h))
        partitions = zip (snd attr) (map (\p -> (h',p)) segregatedVals)
        -}


buildTree :: DataSet -> Attribute -> AttSelector -> DecisionTree 
buildTree ds attr attSelect
    | possibleOutcomesForAtt == ("bad":[]) = Leaf "bad"
    | possibleOutcomesForAtt == ("good":[]) = Leaf "good"
    | possibleOutcomesForAtt == [] = Null
    | otherwise = Node name nextNode
      where
        possibleOutcomesForAtt = nub (map (lookUpAtt (fst attr) (fst ds)) (snd ds))
        partitioned = partitionData ds attr'
        attr'@(name,_) = attSelect ds attr
        nextNode = (map (\(attValue, subData) -> (attValue, buildTree subData attr attSelect)) partitioned)

--------------------------------------------------------------------
-- PART IV
--------------------------------------------------------------------

entropy :: DataSet -> Attribute -> Double
entropy 
  = undefined

gain :: DataSet -> Attribute -> Attribute -> Double
gain 
  = undefined

bestGainAtt :: AttSelector
bestGainAtt 
  = undefined

--------------------------------------------------------------------

outlook :: Attribute
outlook 
  = ("outlook", ["sunny", "overcast", "rainy"])

temp :: Attribute 
temp 
  = ("temp", ["hot", "mild", "cool"])

humidity :: Attribute 
humidity 
  = ("humidity", ["high", "normal"])

wind :: Attribute 
wind 
  = ("wind", ["windy", "calm"])

result :: Attribute 
result
  = ("result", ["good", "bad"])

fishingData :: DataSet
fishingData
  = (header, table)

header :: Header
table  :: [Row]
header 
  =  [outlook,    temp,   humidity, wind,    result] 
table 
  = [["sunny",    "hot",  "high",   "calm",  "bad" ],
     ["sunny",    "hot",  "high",   "windy", "bad" ],
     ["overcast", "hot",  "high",   "calm",  "good"],
     ["rainy",    "mild", "high",   "calm",  "good"],
     ["rainy",    "cool", "normal", "calm",  "good"],
     ["rainy",    "cool", "normal", "windy", "bad" ],
     ["overcast", "cool", "normal", "windy", "good"],
     ["sunny",    "mild", "high",   "calm",  "bad" ],
     ["sunny",    "cool", "normal", "calm",  "good"],
     ["rainy",    "mild", "normal", "calm",  "good"],
     ["sunny",    "mild", "normal", "windy", "good"],
     ["overcast", "mild", "high",   "windy", "good"],
     ["overcast", "hot",  "normal", "calm",  "good"],
     ["rainy",    "mild", "high",   "windy", "bad" ]]

--
-- This is the same as the above table, but with the result in the second 
-- column...
--
fishingData' :: DataSet
fishingData'
  = (header', table')

header' :: Header
table'  :: [Row]
header' 
  =  [outlook,    result, temp,   humidity, wind] 
table' 
  = [["sunny",    "bad",  "hot",  "high",   "calm"],
     ["sunny",    "bad",  "hot",  "high",   "windy"],
     ["overcast", "good", "hot",  "high",   "calm"],
     ["rainy",    "good", "mild", "high",   "calm"],
     ["rainy",    "good", "cool", "normal", "calm"],
     ["rainy",    "bad",  "cool", "normal", "windy"],
     ["overcast", "good", "cool", "normal", "windy"],
     ["sunny",    "bad",  "mild", "high",   "calm"],
     ["sunny",    "good", "cool", "normal", "calm"],
     ["rainy",    "good", "mild", "normal", "calm"],
     ["sunny",    "good", "mild", "normal", "windy"],
     ["overcast", "good", "mild", "high",   "windy"],
     ["overcast", "good", "hot",  "normal", "calm"],
     ["rainy",    "bad",  "mild", "high",   "windy"]]

fig1 :: DecisionTree
fig1
  = Node "outlook" 
         [("sunny", Node "temp" 
                         [("hot", Leaf "bad"),
                          ("mild",Node "humidity" 
                                       [("high",   Leaf "bad"),
                                        ("normal", Leaf "good")]),
                          ("cool", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "temp" 
                         [("hot", Null),
                          ("mild", Node "humidity" 
                                        [("high",Node "wind" 
                                                      [("windy",  Leaf "bad"),
                                                       ("calm", Leaf "good")]),
                                         ("normal", Leaf "good")]),
                          ("cool", Node "humidity" 
                                        [("high", Null),
                                         ("normal", Node "wind" 
                                                         [("windy",  Leaf "bad"),
                                                          ("calm", Leaf "good")])])])]

fig2 :: DecisionTree
fig2
  = Node "outlook" 
         [("sunny", Node "humidity" 
                         [("high", Leaf "bad"),
                          ("normal", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "wind" 
                         [("windy", Leaf "bad"),
                          ("calm", Leaf "good")])]


outlookPartition :: Partition
outlookPartition
  = [("sunny",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","bad"],["hot","high","windy","bad"],
                   ["mild","high","calm","bad"],["cool","normal","calm","good"],
                   ["mild","normal","windy","good"]])),
     ("overcast",([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","good"],["cool","normal","windy","good"],
                   ["mild","high","windy","good"],["hot","normal","calm","good"]])),
     ("rainy",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["mild","high","calm","good"],["cool","normal","calm","good"],
                   ["cool","normal","windy","bad"],["mild","normal","calm","good"],
                   ["mild","high","windy","bad"]]))]