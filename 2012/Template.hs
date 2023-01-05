import Data.List
import Data.Maybe

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process] 
             deriving (Eq, Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

------------------------------------------------------
-- PART I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table
lookUp x
    = fromJust . lookup x

states :: LTS -> [State]
states
    = nub . concatMap (states')
      where
        states' t@((s1,s2),_) = [s1]++[s2]

transitions :: State -> LTS -> [Transition]
transitions state ts
    = [t | t@((s,_),_) <- ts, s == state]

alphabet :: LTS -> Alphabet
alphabet ts
    = [id | (_,id) <- ts]

------------------------------------------------------
-- PART II

actions :: Process -> [Id]
    = nub . actions'
      where
        actions' (Prefix act p)
            = act:actions' p
        actions' (Choice [])
            = []
        actions' (Choice (p:ps))
            = actions' p ++ actions' (Choice ps)
        actions' p 
            = []


accepts :: [Id] -> [ProcessDef] -> Bool
--Pre: The first item in the list of process definitions is
--     that of the start process.
accepts [] _ = True
accepts ids procDefs@(pd:_)
    = True `elem` (accepts' ids (startProc pd))
      where
        accepts' :: [Id] -> Process -> [Bool]
        accepts' [] _ 
            = [True]
        accepts' (id:ids) p
            | not (null nextActs) = concatMap (accepts' ids) (map (snd) nextActs)
            | otherwise = [False]
              where
                nextActs = filter (\(id',_) -> id' == id) (immediateActions p)
                
        startProc (_, Ref p') = lookUp p' procDefs
        startProc p'          = snd p'

        immediateActions :: Process -> [(Id, Process)]
        immediateActions (Choice ps)         = concatMap (immediateActions) ps
        immediateActions (Prefix id (Ref x)) = [(id, lookUp x procDefs)]
        immediateActions (Prefix id p)       = [(id, p)]
        immediateActions (Ref p)             = immediateActions (lookUp p procDefs)
        immediateActions STOP                = []


------------------------------------------------------
-- PART III

--composeTransitions :: Transition -> Transition 
--                   -> Alphabet -> Alphabet 
--                   -> StateMap 
--                   -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions t1@((s,t),a) t2@((s',t'),a') alpha alpha' smap
    | a == a'                 = [((identify_s_s', lookUp (t,t') smap), a)]
    | invalid_a && invalid_a' = []
    | invalid_a'              = [t1']
    | invalid_a               = [t2']
    | otherwise               = [t1',t2']
      where
        invalid_a = a `elem` alpha'
        invalid_a' = a' `elem` alpha
        identify_s_s'    = lookUp (s,s') smap
        t1' = ((identify_s_s', lookUp (t, s') smap), a)
        t2' = ((identify_s_s', lookUp (s, t') smap), a')


pruneTransitions :: [Transition] -> LTS
pruneTransitions ts
    = visit 0 []
      where
        visit :: State -> [State] -> [Transition]
        visit s visited
            | s `elem` visited = []
            | otherwise        = transitions s ts ++ concatMap (`visit` visited') targets
              where 
                targets  = [target | ((_,target),_) <- transitions s ts]
                visited' = s:visited
------------------------------------------------------
-- PART IV

compose :: LTS -> LTS -> LTS
compose 
  = undefined

------------------------------------------------------
-- PART V

buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS 
  = undefined

------------------------------------------------------
-- Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor 
  = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                       Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                       Prefix "off" STOP])

clock 
  = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play 
  = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")), 
                     Prefix "end" STOP])

maker 
  = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user  
  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch 
  = ("SWITCH", Ref "OFF")

off 
  = ("OFF", Choice [Prefix "on" (Ref "ON")])

on  
  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

------------------------------------------------------
-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, makerUserLTS, 
  pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS 
  = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS 
  = [((0,1),"tick"),((1,0),"tock")]

playLTS 
  = [((0,1),"end"),((0,2),"think"),((2,0),"move")]

clockPlayLTS 
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS 
  = [((0,1),"make"),((1,0),"ready")]

userLTS 
  = [((0,1),"ready"),((1,0),"use")]

makerUserLTS 
  = [((0,2),"make"),((2,1),"ready"),((1,0),"use"),((1,3),"make"),((3,2),"use")]

pLTS 
  = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS 
  = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS 
  = [((0,1),"d"),((1,4),"a"),((0,3),"a"),((3,4),"d")]

switchLTS 
  = [((0,1),"on"),((1,0),"off")]


-- Sample StateMap:
m1
  = [((0,0),0),((0,1),1),((1,0),2),((1,1),3)]