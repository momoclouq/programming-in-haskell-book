---------------------
-- Exercise chapter 8. Programming in haskell

-- 1.
data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ n) m = add m (mult n m)   

-- 2.
data Tree a = Leaf a | Node (Tree a) a (Tree a)
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 3) 5 (Leaf 7))

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = case compare x y of
                       LT -> occurs x l
                       EQ -> True
                       GT -> occurs x r


-- 3.
data Treea a = Leafa a | Nodea (Treea a) (Treea a)  

countl :: Treea a -> Int
countl (Leafa _) = 1
countl (Nodea x y) = countl x + countl y

balanced :: Treea a -> Bool
balanced (Leafa _) = True
balanced (Nodea x y) | abs (countl x - countl y) > 1 = False
                     | otherwise = balanced x && balanced y

-- 4.
halves :: [a] -> ([a],[a])
halves x = (take (length x `div` 2) x, drop (length x `div` 2) x)
    

balance :: [a] -> Treea a
balance [a] = Leafa a
balance x = Nodea (balance (fst (halves x))) (balance (snd (halves x)))

-- 5.
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- 6.
t1 :: Expr
t1 = Add (Add (Val 1) (Val 2)) (Val 3)

eval :: Expr -> Int
eval (Val x) = folde (*1) ((+)) (Val x)
eval (Add x y) = eval x + eval y

size :: Expr -> Int
size (Val x) = folde (^0) ((+)) (Val x)
size (Add x y) = size x + size y 

-- 7. book

-- 8.
data Prop = Const Bool
          |Var Char
          |Not Prop
          |And Prop Prop
          |Imply Prop Prop
          |Equiv Prop Prop -- change
          |Or Prop Prop --change

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
eval s (Or p q) = eval s p || eval s q -- change
eval s (Equiv p q) = (eval s (Imply p q)) && (eval s (Imply q p)) --change

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q -- change
vars (Equiv p q) = vars p ++ vars q -- change

-- 9. not yet






