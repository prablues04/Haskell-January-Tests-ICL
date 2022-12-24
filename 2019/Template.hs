module SOL where

import Data.List
import Data.Maybe

import TestData
import Types

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp x list = fromJust (lookup x list) --Is it possible to get rid of x and list from both sides, if so how?

-- 3 marks
vars :: Formula -> [Id]
vars = sort . nub . getVars
    where
      getVars (Var x)   = [x]
      getVars (Not x)   = getVars x
      getVars (And x y) = getVars x ++ getVars y
      getVars (Or x y)  = getVars x ++ getVars y


-- 1 mark
idMap :: Formula -> IdMap
--Pre: Elements of list are already sorted thanks to vars
idMap formula = map (\x -> (x, fromJust (elemIndex x variables) + 1)) variables
    where variables = vars formula

idMap' :: Formula -> IdMap
--Pre: Elements of list are already sorted thanks to vars
idMap' formula = mapIDs variables 1
    where
      variables = vars formula
      mapIDs [] _ = []
      mapIDs (v:vs) x = [(v, x)] ++ mapIDs vs (x+1)

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF (Var x)         = Var x
toNNF (Not (Var x))   = Not (Var x)
toNNF (Not (Not x))   = toNNF x
toNNF (Not (And x y)) = Or (toNNF (Not x)) (toNNF (Not y))
toNNF (Not (Or x y))  = And (toNNF (Not x)) (toNNF (Not y))
toNNF (And x y)       = And (toNNF x) (toNNF y)
toNNF (Or x y)        = Or (toNNF x) (toNNF y)

-- 3 marks
toCNF :: Formula -> CNF
toCNF = simplify . toNNF
    where
      simplify (Var x) = Var x
      simplify (And (Or a b) (Or x y)) = And (Or (simplify a) (simplify b)) (Or (simplify x) (simplify y))
      simplify (And x y) = And (simplify x) (simplify y)
      simplify (Not x)   = Not x
      simplify (Or x y)  = distribute x y

-- 4 marks
flatten :: CNF -> CNFRep
flatten formula = subInFormula formula
    where
      varToNumMap = idMap formula
      subInFormula (Var x)   = [[lookUp x varToNumMap]]
      subInFormula (Not (Var x)) = [[-(lookUp x varToNumMap)]]
      subInFormula (And x y) = subInFormula x ++ subInFormula y
      subInFormula (Or x y)  = [a++b]
          where
            ([a],[b]) = (subInFormula x, subInFormula y)

--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits [] = ([],[])
propUnits cnfrep = (fst (propagate cnfrep''), snd (propagate cnfrep'') ++ units)
    where
      (cnfrep', units) = propagate cnfrep
      cnfrep'' = simplifyFully (cnfrep', units)

      simplifyFully :: (CNFRep, [Int]) -> CNFRep
      simplifyFully (formula,[]) = formula
      simplifyFully (formula, u:us)
          = simplifyFully (deleteFromCNF cnfrep' u , us)
      
      propagate :: CNFRep -> (CNFRep,[Int])
      propagate [] = ([],[])
      propagate (x@(y:ys):xs)
          | ys == []  = (xs,x ++ snd (propagate xs))
          | otherwise = (x:fst (propagate xs), [] ++ snd (propagate xs))
      
      deleteFromCNF :: CNFRep -> Int -> CNFRep
      deleteFromCNF [] _ = []
      deleteFromCNF (c:cs) atom
          | atom `elem` c  = deleteFromCNF cs atom
          | atom' `elem` c = (delete atom' c):deleteFromCNF cs atom
          | otherwise      = c:deleteFromCNF cs atom
            where atom' = (-1) * atom


-- 4 marks
dp :: CNFRep -> [[Int]]
dp 
  = undefined

--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat
  = undefined

