halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve xs | length(xs) `mod`  2 == 0 = (take (length(xs) `div` 2) xs , drop (length(xs) `div` 2) xs)
         | otherwise = (take (length(xs) `div` 2) xs, drop (length(xs) `div` 2) xs)

third :: [a] -> a
third [x] = x
third [x , _] = x
third (_ : _ : x: _) = x

safetail :: [a] -> [a]
safetail [] = []
safetail xs = tail xs
-- safetail xs | not (null xs) = tail xs
--           | otherwise = []
-- safetail xs = if not (null xs) then tail xs else [] 

(||*) :: Bool -> Bool -> Bool
True ||* False = True
True ||* True = True
False ||* False = False
False ||* True = True

and1 :: Bool -> Bool -> Bool
and1 a b = if a then b else False

mult :: Int -> Int -> Int -> Int
mult = \x -> (\y ->(\z -> x*y*z))

luhnDouble :: Int -> Int
luhnDouble x | 2*x > 9 = 2*x - 9
             | x <= 0 = 0
             | otherwise = 2*x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn = \x -> (\y -> (\z -> (\k -> (k + luhnDouble z + y + luhnDouble x) `mod` 10 == 0)))

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

-- --------------------------------------
-- chapter 5. list comprehensions

-- zip function
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

lowers :: String -> Int
lowers xs = length [ x | x <- xs , x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [ x' | x' <- xs, x' == x]

-- Exercises

-- 1. Book
-- 2.
grid :: Int -> Int -> [(Int,Int)]
grid x y | x <= 0 || y <= 0 = [(0,0)]
         | otherwise = [(x',y') | x' <- [0 .. x], y' <- [0 .. y]]
-- 3.
square :: Int -> [(Int, Int)]
square x | x <= 0 = [(0,0)]
         | otherwise = [k | k <- grid x x, k /= (0,0) && k /= (x,x)]
-- 4.
replicatex :: Int -> a -> [a]
replicatex x k | x <= 0 = replicate 1 k
               | otherwise = [k | _ <- [1 .. x]]
-- 5.
pyths :: Int -> [(Int, Int, Int)]
pyths x | x <= 0 = [(0,0,0)]
        | otherwise = [(a,b,c) | a <- [1 .. x], b <- [1 .. x], c <- [1..x], a^2 + b^2 == c^2]
-- 6.
perfects :: Int -> [Int]
perfects x | x <= 0 = [0]
           |otherwise = [a | a <- [1..x], sum (factors a) - a == a]
-- 7.
-- concat [ (x,3) : [(x,4)] | x <- [1,2]]
-- 8.
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions2 :: Eq a => a -> [a] -> [Int]
positions2 x xs = [i | i <- find x (zip xs [0..])]

-- --------------------------------------
-- chapter 6. recursive functions
fac :: Int -> Int
fac 0 = 1
fac n | n < 0 = 1
      |otherwise = n * fac (n-1)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (n:xs) | x <= n = x:n:xs
                | otherwise = n : insert x xs

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x-2) + fib (x-1)
-- mutal recursion
even1 :: Int -> Bool
even1 0 = True
even1 n = odd1 (n-1)

odd1 :: Int -> Bool
odd1 0 = False
odd1 n = even1 (n-1)

-- exercises
-- 2.
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- 3. 
(^|) :: Int -> Int -> Int
m ^| 0 = 1
m ^| n = m * (m ^| (n-1))
-- 2^3 = 2*(2^2)= 2*(2*(2^1)) = 2*(2*(2*(2^0))) = 2 * 2 * 2 * 1 = 8

--4. 
euclid :: Int -> Int -> Int
euclid a b | a == b = a
           | a > b = euclid (a-b) b
           | otherwise = euclid a (b-a)

-- 5. book

-- 6.
and2 :: [Bool] -> Bool
and2 [] = True
and2 (x:xs) | x == True = and2 xs
            | otherwise = False

concat2 :: [[a]] -> [a]
concat2 []= []
concat2 (xs:xss) = xs ++ concat2 xss

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a : replicate' (n-1) a

(!!|) :: [a] -> Int -> a
(x:xs) !!| 0 = x
(x:xs) !!| n = xs !!| (n-1) 

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) | a == x = True
               | otherwise = elem' a xs

-- 7.
merge :: Ord a => [a] -> [a] -> [a]
merge [] a = a
merge a [] = a
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- 8.
halve1 :: [a] -> ([a],[a])
halve1 [] = ([],[])
halve1 a = (take (length a `div` 2) a, drop (length a `div` 2) a) 

msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort a = merge (msort (fst (halve1 a))) (msort (snd (halve1 a)))

