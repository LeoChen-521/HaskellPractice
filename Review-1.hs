g (1:xs) = 3:g xs
g (x:xs) | x>1       = g (x-1:xs)
         | otherwise = []

{-
> g [3,1,0,4,5]
[3,3]
> g [5,1,2,-3,5]
[3,3,3]
-}


{-
Which of the following data type definitions are accurate abstract syntax representations for binary numbers?

1 data Bin = A | B | Conc Bin Bin

2 data Bin = Seq Bool [Bool]

3 data Bin = OnlyZero | OnlyOne
           | ZeroAnd Bin | OneAnd Bin

4 data Bin = Single Bool | Many Bool Bin Bool

5 data Bin = Many [Bool]

6 data Bin = Single Bool | Many Bool Bin

7 type Bin = (Bool,[Bool])

    10   ~  (True,[False])
    0    ~  (False,[])
    110  ~  (True,[True,False])

8 type Bin = [Bool]

>>> 1,2,3,(5),6,7,(8)
>>> NOTE: 5 and 8 are strictly speaking incorrect,
but are acceptable from a pragmatic perspective
-}


{-
num ::= ... numbers
var ::= ... strings

expr ::= num | var | expr + expr | expr * expr

stmt ::= var ":=" expr
      |  stmt ";" stmt
      |  "WHILE" expr "DO" stmt
      -- More practice:
      |  "SWAP" Name Name
      |  "FOR" expr stmt     -- expr must be var
-}

type Name = String

data Expr = Lit Int
          | Var Name
          | Plus Expr Expr
          | Mult Expr Expr
          deriving Show

data Stmt = Assign Name Expr
          | Seq [Stmt]
          | While Expr Stmt
          deriving Show

type State = [(Name,Int)]

ref :: Name -> State -> Int
ref v s = head ([i | (w,i) <- s, w==v] ++ [0])

upd :: Name -> Int -> State -> State
upd v i s = (v,i):[(w,j) | (w,j) <- s, w/=v]

type D_Expr = State -> Int
type D_Stmt = State -> State

-- semE :: Expr -> D_Expr
semE :: Expr -> State -> Int
semE (Lit i) _ = i
semE (Var x) s = ref x s
semE (Plus e1 e2) s = semE e1 s + semE e2 s
semE (Mult e1 e2) s = semE e1 s * semE e2 s

-- semS :: Stmt -> D_Stmt
semS :: Stmt -> State -> State
semS (Assign v e) s = upd v (semE e s) s
semS (Seq [])     s = s
semS (Seq (t:ts)) s = semS (Seq ts) (semS t s)
semS (While e b)  s
     | semE e s==0 = s
     | otherwise   = semS (While e b) (semS b s)


run :: Stmt -> State
run s = semS s []

[n,f,x,y] = map Var ["n","f","x","y"]

fac :: Int -> Stmt
fac x = Seq [
    Assign "n" (Lit x),
    Assign "f" (Lit 1),
    While n (Seq [
        -- Assign "f" (Mult (Var "f") (Var "n"))
        Assign "f" (Mult f n),
        Assign "n" (Plus n (Lit (-1))) ])]
