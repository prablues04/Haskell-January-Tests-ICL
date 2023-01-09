module SC where

import Data.List
import Data.Maybe

import Types
import Examples

---------------------------------------------------------

prims :: [Id]
prims
  = ["+", "-", "*", "<=", "ite"]

lookUp :: Id -> [(Id, a)] -> a
lookUp v env
  = fromMaybe (error ("lookUp failed with search key " ++ v))
              (lookup v env)

---------------------------------------------------------
-- Part I

isFun :: Exp -> Bool
isFun (Fun _ _) 
    = True
isFun _
    = False

splitDefs :: [Binding] -> ([Binding], [Binding])
--Pre: each expression has a binding - e.g.. would not work for e6 as e6 does not have a Let [Binding] clause
splitDefs 
    = partition (\(_, expr) -> isFun expr) 

topLevelFunctions :: Exp -> Int
topLevelFunctions (Let bs _)
    = foldr (+) 0 isFunMapping
      where isFunMapping = map (\(_,expr) -> if isFun expr then 1 else 0) bs
topLevelFunctions _
    = 0
---------------------------------------------------------
-- Part II

unionAll :: Eq a => [[a]] -> [a]
unionAll
    = nub . foldr (union) []

freeVars :: Exp -> [Id]
freeVars (Const _)
    = []
freeVars (Var x)
    | x `elem` prims = []
    | otherwise      = [x]
freeVars (Fun ps e)
    = freeVars e \\ ps
freeVars (App f es)
    = unionAll (map freeVars es ++ [freeVars f]) 
freeVars (Let bs e)
    = unionAll ((map freeVars es ++ [freeVars e]) \\ fNames)
      where
        (fs,_) = splitDefs bs
        fNames = map fst fs
        es = map snd fs
---------------------------------------------------------
-- Part III

-- Given...
lambdaLift :: Exp -> Exp
lambdaLift e
  = lift (modifyFunctions (buildFVMap e) e)

buildFVMap :: Exp -> [(Id, [Id])]
buildFVMap 
  = undefined

modifyFunctions :: [(Id, [Id])] -> Exp -> Exp
-- Pre: The mapping table contains a binding for every function
-- named in the expression.
modifyFunctions 
  = undefined

-- The default definition here is id.
-- If you implement the above two functions but not this one
-- then lambdaLift above will remove all the free variables
-- in functions; it just won't do any lifting.
lift :: Exp -> Exp
lift 
  = id

-- You may wish to use this...
lift' :: Exp -> (Exp, [Supercombinator])
lift' 
  = undefined