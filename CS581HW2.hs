-- Name: Xiaoru Chen (ID:934339052), chenxia2@oregonstate.edu
-- CS581
-- Instructor: Martin Erwig
-- 2022.02.02


module CS581HW2 where

--Disregard built in name "Num"
import Prelude hiding (Num)

-- Exercise 1. Mini Logo
-- (a) Define the abstract syntax for Mini Logo as a Haskell data type Cmd

-- Variable names
type Name = String
type Pars = [Name]

type Num = Int
type Vals = [Num]

data Pos = I Num | S Name
	deriving (Eq,Show)

-- The mode of the pen.
data Mode = Up | Down
	deriving (Eq,Show)

data Cmd = Pen Mode
         | Moveto (Pos, Pos)
         | Def Name (Pars) Cmd
         | Call Name (Vals)
         | Seq [Cmd] -- Seq Cmd Cmd
	deriving (Eq,Show)

-- (b) Write a Mini Logo function vector that draws a line from a given position (x1,y1) to
--     a given position (x2, y2) and represent the function in abstract syntax, that is, as 
--     a Haskell value of type Cmd in (a).

-- if x1, y1, x2, y2 are strings
vector = Def "vector" (["x1", "y1", "x2", "y2"]) (Seq
	               [Pen Up, 
	                Moveto (S "x1", S "y1"),
	                Pen Down,
	                Moveto (S "x2", S "y2"),
	                Pen Up])

-- if x1, y1, x2, y2 are integer
--vector = Def "vector" ([x1, y1, x2, y2]) (Seq
--	               [Pen Up, 
--	                Moveto (I x1, I y1),
--	                Pen Down,
--	                Moveto (I x2, I y2),
--	                Pen Up])


-- (c) Define a Haskell function steps :: Int -> Cmd that constructs a Mini Logo program
--     which draws a stair of n steps. The function steps is a program generator.


--Cmd can also be a list of Cmd
steps :: Int -> Cmd
steps 0 = Seq [Pen Up, Moveto (I 0, I 0)]
steps 1 = Seq [Pen Up, Moveto (I 1, I 1), Pen Down, Moveto (I 0, I 1), Moveto (I 0, I 0)]
steps n = Seq [Pen Up, Moveto (I n, I n), Pen Down, Moveto (I (n-1), I n), Moveto (I (n-1), I (n-1)), steps (n-1)]


-- Exercise 2. Grammar Grammar

-- (a) Give Haskell data type definitions for types Grammar, Prod, RHS, and Symbol, to represent
--     the abstract syntax for the above language.

type NT = String
type Term = String

data Symbol = S1 NT | S2 Term
	deriving (Eq,Show)
data RHS = Seq0 [Symbol]
	deriving (Eq,Show)
data Prod = P NT [RHS]
	deriving (Eq,Show)
data Grammar = Seq1 [Prod]
	deriving (Eq,Show)

-- (b) 
--data Cond = T
--          | Not Cond
--	deriving (Eq,Show)

--data Stmt = Skip
--          | While Cond Stmt
--          | Seq3 Stmt Stmt
--	deriving (Eq,Show)

impcond0 :: RHS
impcond0 = Seq0 [S1 "cond", S2 "T", S2 "not", S2 "(", S2 ")"]

impcond1 :: Prod
impcond1 = P "cond" [impcond0]

impstmt0 :: RHS
impstmt0 = Seq0 [S1 "stmt", S1 "cond", S2 "skip", S2 "while", S2 "do", S2 ";", S2 "{", S2 "}"]

impstmt1 :: Prod
impstmt1 = P "stmt" [impstmt0]

--impstmt2 :: Prod
--impstmt2 = P "cond" [impstmt0]

imp :: Grammar
imp = Seq1 [impcond1, impstmt1]

-- (c) 

-- Extracting all defined nonterminals
nonterminals :: Grammar -> [NT]
nonterminals (Seq1 []) = []
nonterminals (Seq1 (P nt l:ps)) = [nt] ++ nonterminals (Seq1 ps)

-- Extracting all all used terminals
getstringsym :: Symbol -> String
getstringsym (S1 str) = str
getstringsym (S2 str) = str

getstringsymlist :: [Symbol] -> [String]
getstringsymlist [] = []
getstringsymlist (s:ss) = getstringsym s : getstringsymlist (ss)

getstringsrhs :: RHS -> [String]
getstringsrhs (Seq0 []) = []
getstringsrhs (Seq0 symlist) = getstringsymlist symlist

getstringsrhslist :: [RHS] -> [String]
getstringsrhslist [] = []
getstringsrhslist (r:rs) = getstringsrhs r ++ getstringsrhslist (rs)

getstringsprod :: Prod -> [String]
getstringsprod (P nt []) = []
getstringsprod (P nt rhslist) = getstringsrhslist rhslist

getstringsprodlist :: [Prod] -> [String]
getstringsprodlist [] = []
getstringsprodlist (p:ps) = getstringsprod p ++ getstringsprodlist (ps)

getstringsgrammer :: Grammar -> [String]
getstringsgrammer (Seq1 prodlist) = getstringsprodlist prodlist

isinlist :: String ->[String] -> Bool
isinlist [] [] = True
isinlist str [] = False
isinlist str (h:t) = if str == h then True else isinlist str (t)

-- Construct a list of strings from two lists of strings, 
-- and select strings from the first list except strings from the second list
pick :: [String] -> [String] -> [String]
pick [] _ = []
pick _ [] = []
pick (h:t) l2 = if isinlist h l2 == False then [h] ++ pick t l2 else pick t l2

-- Extracting all used terminals
terminals :: Grammar -> [Term]
terminals (Seq1 []) = []
terminals g = pick (getstringsgrammer g) (["cond","stmt"])












