-- Name: Xiaoru Chen (ID:934339052), chenxia2@oregonstate.edu
-- CS581
-- Instructor: Martin Erwig
-- 2022.02.13


module CS581HW3 where

import Prelude hiding (Assign)

-- Exercise 1. Imperative Language
type Name = String
data Fun = Succ | Add Name
     deriving (Eq,Show)
data Stmt = Assign Name Int | Apply Fun Name | Twice Stmt
     deriving (Eq,Show)
type Prog = [Stmt]
type State = [(Name, Int)]

getname :: (Name, Int) -> Name
getname (n, _) = n

getval :: (Name, Int) -> Int
getval (_, i) = i

-- Check if a Name is already exist in a State
--checkexistname :: State -> Name-> Bool
--checkexistname [] n     = False
--checkexistname (x:xs) n = if getname x == n then True else checkexistname xs n

-- Retrieving the value of a variable name from the state.
-- 输入一个名字，找到state里面这个名字的数值。Name在这个state里面等于多少。
val :: State -> Name -> Int
val [] n     = 0
val (x:xs) n = if getname x == n then getval x else val xs n

--getheadname :: State -> Name
--getheadname (x:xs) = getname x

-- replace function
replaceval :: (Name, Int) -> Int -> (Name, Int)
replaceval (n, v) i = (n, i)

-- Insert a "combination" into a state, 
-- if the Name is already in the state, use replace function to replace it.
insert :: State -> (Name, Int) -> State
insert [] (n, i)     = [(n, i)]
insert (x:xs) (n, i) = if getname x == n then (replaceval x i) : xs else x : insert xs (n, i)

-- Apply a stmt to a State and get a new State
semStmt :: Stmt -> State -> State
semStmt (Assign n i) s        = insert s (n, i)
semStmt (Apply Succ n) s      = insert s (n, (val s n) + 1)
semStmt (Apply (Add n1) n2) s = if val s n1 == 0 && val s n2 == 0 then insert s (n2, 0)
                                else if val s n1 /= 0 && val s n2 == 0 then insert s (n2, val s n1)
                                else insert s (n2, (val s n2) + (val s n1))
semStmt (Twice stmt) s        = semStmt stmt (semStmt stmt s)

getState :: Prog -> State -> State
getState [] s = s
getState (x:xs) s = getState xs (semStmt x s)

-- Apply a list of stmt to a State and get a new State
-- ??? semProg should call semStmt with an empty state (that is, a state in which no variable has been defined)
semProg :: Prog -> State
--         [stmt]-> [(name, Int)]
semProg [] = []
semProg p = getState p []

-- We can also change semProg to the following code,
-- It has two arguments Prog and a []
-- Professor agreed with the semProg with 1 argument as shown above.
-- Always starts at empty state.

--semProg :: Prog -> State -> State
--semProg [] s = s
--semProg (x:xs) s = semProg xs (semStmt x s)

---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
-- Exercise 2. Mini Logo

data Cmd = Pen Mode
         | MoveTo Int Int
         | Sequ Cmd Cmd
     deriving (Eq,Show)

data Mode = Up | Down
     deriving (Eq,Show)

-- 当前笔的状态，当前笔的坐标
-- English: Pen's current Mode, and pen's current coordinate
type State2 = (Mode,Int,Int)

-- Semantic domain: Representing a set of drawn lines
--                  Represented by the type Lines. 

-- 1 line has two points, and 4 Int
type Line = (Int,Int,Int,Int)
type Lines = [Line]

getCurrentState :: (State2, Lines) -> State2
getCurrentState (s, _) = s

getCurrentLines :: (State2, Lines) -> Lines
getCurrentLines (_, l) = l

-- One Cmd modifies the current state and produces lines
sem :: Cmd -> State2 -> (State2,Lines)
sem (Pen Up) (_, x1, y1) = ((Up, x1, y1), [])           -- produces no lines
sem (Pen Down) (Up, x1, y1)   = ((Down, x1, y1), [])    -- produces no lines
sem (Pen Down) (Down, x1, y1) = ((Down, x1, y1), [])    -- produces no lines
sem (MoveTo x2 y2) (Up, x1, y1)   = ((Up, x2, y2), [])  -- produces no lines
sem (MoveTo x2 y2) (Down, x1, y1) = ((Down, x2, y2), [(x1, y1, x2, y2)])
sem (Sequ c1 c2) s = sem c2 (getCurrentState (sem c1 s))

-- The function run should call sem. 
-- The initial state is defined to have the pen up 
-- and the current drawing position at (0, 0).
run :: Cmd -> Lines
run c = getCurrentLines (sem c (Up, 0, 0))

---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
-- Exercise 3. Stack Language


type S = [Op]
data Op = LD Int | ADD | SWAP | DUP
     deriving (Eq,Show)

type Stack = [Int]

type D = Stack -> Maybe Stack

getStackFirst :: Stack -> Int 
getStackFirst (x:_) = x

getStackSecond :: Stack -> Int
getStackSecond (x:xs) = getStackFirst xs

getStackLength :: Stack -> Int
getStackLength [] = 0
getStackLength (x:xs) = getStackLength xs + 1

removeTop :: Stack -> Stack
removeTop [] = []
removeTop (x:xs) = xs

--       Op -> Stack -> Maybe Stack
semOp :: Op -> D

semOp (LD i) s = Just (i : s)

semOp (ADD) s  = if (getStackLength s) >= 2 
                 then Just ((getStackFirst s + getStackSecond s) : (removeTop (removeTop s))) 
                 else Nothing

semOp (SWAP) s = if (getStackLength s >= 2)
                 then Just ((getStackSecond s) : ((getStackFirst s) : (removeTop (removeTop s))))
                 else Nothing

semOp (DUP) s = if (getStackLength s > 0)
                then Just ((getStackFirst s) : s)
                else Nothing

removeJust :: Maybe Stack -> Stack
removeJust (Just l)  = l

--   [Op] -> Stack -> Maybe Stack    semS opl = getStack opl
--semS :: S -> D
--semS opl = getMaybeStack opl [] 

semS :: [Op] -> Stack -> Maybe Stack
semS [] s = Just s
semS (x:xs) s = if semOp x s == Nothing then Nothing else semS xs (removeJust (semOp x s))








































