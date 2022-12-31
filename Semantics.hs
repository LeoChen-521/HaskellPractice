module Semantics where


--
-- Exercise 1
--
type Name = String
data Fun  = Succ | Add Name deriving Show
data Stmt = Assign Name Int | Apply Fun Name | Twice Stmt deriving Show
type Prog = [Stmt]

type Memory = [(Name,Int)]

clear :: Name -> Memory -> Memory
clear x m = [d | d <- m, fst d/=x]

val :: Name -> Memory -> Int
val x [] = 0
val x ((y,i):m) | y==x      = i
                | otherwise = val x m

semStmt :: Stmt -> Memory -> Memory
semStmt (Assign x i)      m = (x,i):clear x m
semStmt (Apply Succ x)    m = (x,val x m+1):clear x m
semStmt (Apply (Add y) x) m = (x,val x m+val y m):clear x m
semStmt (Twice s)         m = semStmt s (semStmt s m)

semProg :: Prog -> Memory -> Memory
semProg []    m = m
semProg (s:p) m = semProg p (semStmt s m)

p1 = [Apply Succ "x"]
p2 = [Assign "x" 5]
p3 = [Assign "x" 5,Assign "y" 2,Apply (Add "x") "y"]
p4 = p2 ++ [Apply Succ "x"]
p5 = p2 ++ [Apply (Add "x") "x"]
p6 = [Twice (Apply Succ "x")]
p7 = p2 ++ [Twice (Apply (Add "x") "x")]

testsProg  = [p1,p2,p3,p4,p5,p6,p7]
expectProg = [[("x",1)], [("x",5)], [("y",7),("x",5)],
              [("x",6)], [("x",10)], [("x",2)], [("x",20)]]
testProg = [semProg p [] | p <- testsProg] == expectProg


--
-- Exercise 2
--
data Cmd = Pen Mode
         | MoveTo Int Int
         | Sequ Cmd Cmd
         deriving Show

data Mode = Up | Down deriving Show

type Line  = (Int,Int,Int,Int)
type Lines = [Line]
type State = (Mode,Int,Int)

sem :: Cmd -> State -> (State,Lines)
sem (Pen Up)     (_   ,x,y) = ((Up,x,y),   [])
sem (Pen Down)   (_   ,x,y) = ((Down,x,y), [])
sem (MoveTo x y) (Up  ,_,_) = ((Up,x,y),   [])
sem (MoveTo x y) (Down,u,v) = ((Down,x,y), [(u,v,x,y)])
sem (Sequ c1 c2) s          = (s2,         l1++l2)
                            where
                                (s2,l2) = sem c2 s1
                                (s1,l1) = sem c1 s

run :: Cmd -> Lines
run c = snd (sem c (Up,0,0))

testCmd = run (MoveTo 2 3 `Sequ` Pen Down `Sequ` MoveTo 4 5) == [(2,3,4,5)]



--
-- Exercise 3
--
data Op = LD Int | ADD | SWAP | DUP deriving Show
type S = [Op]


type Stack = [Int]
type D = Stack -> Maybe Stack

semOp :: Op -> Stack -> Maybe Stack
semOp (LD i) s       = Just (i:s)
semOp DUP    (i:s)   = Just (i:i:s)
semOp ADD    (i:j:s) = Just (i+j:s)
semOp SWAP   (i:j:s) = Just (j:i:s)
semOp _      _       = Nothing

semS :: S -> Stack -> Maybe Stack
semS []    s = Just s
semS (o:p) s = case semOp o s of
                    Just s' -> semS p s'
                    Nothing -> Nothing


tst1 = [LD 3, DUP, ADD, DUP, SWAP]
tst2 = []::S
err1 = [LD 3, ADD]
err2 = [LD 3, SWAP]
err3 = [ADD]
testsStack  = [tst1,       tst2,    err1,    err2,    err3]
expectStack = [Just [6,6], Just [], Nothing, Nothing, Nothing]
testStack = [semS s [] | s <- testsStack] == expectStack

{-
type D = Maybe Stack -> Maybe Stack

semOp :: Op -> Maybe Stack -> Maybe Stack
semOp (LD i) (Just s)       = Just (i:s)
semOp DUP    (Just (v:s))   = Just (v:v:s)
semOp ADD    (Just (i:j:s)) = Just (i+j:s)
semOp SWAP   (Just (i:j:s)) = Just (j:i:s)
semOp _      _              = Nothing

semS :: S -> Maybe Stack -> Maybe Stack
semS []     s = s
semS (c:cs) s = semS cs (semOp c s)


tst1 = [LD 3, DUP, ADD, DUP, SWAP]
tst2 = []::S
err1 = [LD 3, ADD]
err2 = [LD 3, SWAP]
err3 = [ADD]
testsStack  = [tst1,       tst2,    err1,    err2,    err3]
expectStack = [Just [6,6], Just [], Nothing, Nothing, Nothing]
testStack = [semS s (Just []) | s <- testsStack] == expectStack
-}
