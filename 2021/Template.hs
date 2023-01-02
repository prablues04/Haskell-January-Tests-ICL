module Alloc where

import Data.Maybe
import Data.List

import Types
import Examples

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count x list 
    = foldr (+) 0 [if x == e then 1 else 0 | e <- list]
--count x xs = length (filter (\lambda -> lambda == x) xs)

degrees :: Eq a => Graph a -> [(a, Int)]
degrees graph@(nodes,edges) = map (degrees') nodes
    where
        degrees' node = (node,count node (map fst edges) + count node (map (snd) edges))

neighbours :: Eq a => a -> Graph a -> [a]
neighbours n (_,edges) 
    = [nb | (nb,x) <- edges, n == x] ++ [nb | (x,nb) <- edges, n == x]

removeNode :: Eq a => a -> Graph a -> Graph a
removeNode n graph@(nodes,edges) 
    = (filter (/=n) nodes, filter (\e -> (fst e)/=n && (snd e)/=n) edges)

------------------------------------------------------
--
-- Part II
--
colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph _ ([],_) = []
colourGraph numColours g@(nodes, edges) 
    | numConnections == [1] = [(n,nColour)] ++ initMap
    | otherwise             = [(n,nColour)] ++ colourGraph'
    where
        colourGraph' = colourGraph numColours g'
        (_,n) = minimum (map (\(a,b)->(b,a)) (degrees g))
        g'@(ns',_) = removeNode n g
        numConnections = nub (map (snd) (degrees g'))
        initMap = zip (coloured) [1..numColours] ++ map (\n' -> (n',0)) (blanks)
        (coloured, blanks) = splitAt numColours ns'
        (nColour:_) = ([1..numColours] \\ map (`lookUp` colourGraph') neighbours_n) ++ [0]
        neighbours_n = neighbours n g




------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap cMap
    = [("return", "return")] ++ coloured ++ blanks
    where
        (colouredNodes,blankNodes) = partition (\(nodes, colour) -> colour /= 0) cMap
        coloured = map(\(n,c) -> (n, "R" ++ show c)) colouredNodes
        blanks   = map(\(n,c) -> (n,n)) blankNodes

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments args mapping = assignments
    where
        mapping' = filter (\(n,reg) -> n `elem` args) mapping
        assignments = map (\(n,reg) -> Assign reg (Var n)) mapping'

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp (Var x) mapping = Var (lookUp x mapping)
renameExp (Const x) _     = (Const x)
renameExp (Apply op e1 e2) mapping 
    = Apply op (renameExp e1 mapping) (renameExp e2 mapping)

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock [] _ = []
renameBlock ((Assign id exp):s') mapping 
    | Var idReg == renamedExp = renameBlock s' mapping
    | otherwise               = Assign idReg renamedExp : renameBlock s' mapping
        where
            idReg = lookUp id mapping
            renamedExp = renameExp exp mapping
renameBlock ((If exp b1 b2):s') mapping
    = If (renameExp exp mapping) (renameBlock' b1) (renameBlock' b2) : renameBlock' s'
        where renameBlock' b = renameBlock b mapping
renameBlock ((While exp block):s') mapping
    = While (renameExp exp mapping) (renameBlock block mapping) : renameBlock s' mapping

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG 
  = undefined

-----------------------------------------------------
--
-- Part V
--
liveVars :: CFG -> [[Id]]
liveVars 
  = undefined

buildCFG :: Function -> CFG
buildCFG 
  = undefined