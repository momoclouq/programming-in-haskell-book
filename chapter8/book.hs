-------------------------
-- Chapter 8. Declaring types and classes

--1. delarations
type String = [Char]
type Pos = (Int,Int)
type Trans = Pos -> Pos
type Pair a = (a,a)
type Assoc k v = [(k,v)]
-- implementation
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

-- 2. data declarations
data Move = North | South |East | West deriving Show
move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x, y-1)
move East (x,y) = (x+1, y)
move West (x,y) = (x-1, y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (x:xs) p = moves xs (move x p)

rev :: Move -> Move
rev North = South
rev South = North
rev West = East
rev East = West
--
data Shape = Circle Float | Rect Float Float
square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

-- 3. Newtype
newtype Nat = N Int
 -- 4. Recursive type
data Nat1 = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ (n)) m = Succ (add n m)

data List a = Nil | Cons a (List a)
len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-- 5. Class and instance declarations
-- example: book

-- Tautology checker
data Prop = Const Bool
          |Var Char
          |Not Prop
          |And Prop Prop
          |Imply Prop Prop
          |Equiv Prop Prop
          |Or Prop Prop

p1 :: Prop
p1 = And (Var `A`) (Not (Var `A`))

p2 :: Prop
p2 = Imply (And (Var `A`) (Var `B`)) (var `A`)

p3 :: Prop
p3 = Equiv (And (Var `A`) (Or (Var `B`) (Var `A`)) ) p2

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Equiv p q) = (eval s (Imply p q)) && (eval s (Imply q p))

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
         where bss = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p| s <- substs p]

-- Abstract machine
data Expr = Val Int | Add Expr Expr
value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y

type Cont = [Op]

data Op = EVAL Expr | ADD Int

eval' :: Expr -> Cont -> Int
eval' (Val x)  c = exec c n
eval' (Add x y) c  = eval' x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (n+m)

value :: Expr -> Int
value e = eval e p[]
