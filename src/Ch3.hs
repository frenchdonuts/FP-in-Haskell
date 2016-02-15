module Ch3
    ( f
    , tail'
    , setHead
    , drop'
    , dropWhile'
    , init'
    , length'
    , sum''
    , product''
    , length''
    , reverse'
    , reverse''
    , flatten
    , append
    , foldRight
    , addOne
    , map'
    , filter'
    , flatMap
    , filter''
    , addLists
    , zipWith'
    , subsequences
    , exampleTree
    , size
    , maximum'
    , depth
    , mapT
    , foldT
    , size'
    , maximum''
    , depth'
    , mapT'
    ) where

-- Ex 3.1
-- Suppose we have a fn:
f (x : (2 : (4 : _))) = x
f [] = 42
f (x : (y : (3 : (4 : _)))) = x + y
f (h : t) = h + sum(t)
f _ = 101
-- What will f [1,2,3,4,5] evaluate too?
-- My answer: 3
-- Answer: 3

-- Ex 3.2
tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs
-- I think tail' [] = undefined also makes sense as an implementation

-- Ex 3.3
setHead :: a -> [a] -> [a]
setHead x [] = [x]
setHead x (y:ys) = (x:ys)

-- Ex 3.4
drop' :: [a] -> Int -> [a]
drop' [] _ = []
drop' xs 0 = xs
drop' xs n = drop' (tail xs) (n-1)

-- Ex 3.5
dropWhile' :: [a] -> (a -> Bool) -> [a]
dropWhile' [] _ = []
dropWhile' (x:xs) pred
    | pred x = dropWhile' xs pred
    | otherwise = x:xs

-- Ex 3.6
init' :: [a] -> [a]
init' (x:[]) = []
init' (x:xs) = x : (init' xs)
-- This fn. can't be implemented in constant time b/c of the way the
-- expression is structured: (x1 : (x2 : (x3 : [])))...not sure what else
-- to say.

-- Ex 3.7
-- I don't think product can be short-circuited if it's being implemented
-- using foldRight. The only place to put the short-circuiting logic is in
-- the function you pass into foldRight, and that function can't affect the
-- "global" behaviour of foldRight. It's only responsible for determining
-- what happens to at...<Not sure what to say here>
-- If we could add a notion of a "fixed point" to foldRight...

-- Ex 3.8
-- Passing in [] and (:) into foldRight will copy the list. All I can say
-- is that there seems to be a deep relationship between foldRight and the
-- data constructors of List

-- Ex 3.9
length' :: [a] -> Integer
length' = foldr toOne 0
    where toOne _ acc = 1 + acc

-- Ex 3.10
foldLeft :: (b -> a -> b) -> b -> [a] -> b
foldLeft _ z [] = z
foldLeft f z (x:xs) = foldLeft f (f z x) xs

-- Ex 3.11
sum'' :: [Integer] -> Integer
sum'' = foldLeft (+) 0 

product'' :: [Integer] -> Integer
product'' = foldLeft (*) 1

length'' :: [Integer] -> Integer
length'' = foldLeft addOne 0
    where addOne acc _ = acc + 1

-- Ex 3.12
reverse' :: [a] -> [a]
reverse' = foldr f []
    where f x acc = acc ++ [x]

reverse'' :: [a] -> [a]
reverse'' (x:xs) = foldl f [x] xs
    where f acc x = x : acc
-- The foldl version is easy to understand...but the foldr...I cheated by
-- writing reverse' using straight recursion. Then, pattern matching.
-- Actually, the answer key only uses foldl.

-- Ex 3.13 (Go through the derivation in Real World Haskell once you've
-- come up w/ your own answer)
foldLeft'' :: (b -> a -> b) -> b -> [a] -> b
foldLeft'' = undefined
          
foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight f z = foldl f1 z
    where f1 = undefined

-- Ex 3.14
-- Using foldr is easy here. We're just mapping the [] of xs1 to xs2
append :: [a] -> [a] -> [a]
append xs1 xs2 = foldr f xs2 xs1
    where f x acc = x : acc
-- Using foldl is tricky. Haven't figure out how to do it w/o using reverse
-- on xs1 first. Even the answer key doesn't implement append using
-- foldLeft

-- Ex 3.15
flatten :: [[a]] -> [a]
flatten = foldl (++) []
-- Hmmm...this ^solution's runtime is not "linear in the total length of all
-- lists," b/c (++)'s runtime is proportional to it's left argument. In this
-- case, the left argument is the accumulator, which is getting longer and
-- longer as we go through each list. The answer key uses foldr...
flatten' :: [[a]] -> [a]
flatten' = foldr (++) []
-- b/c now the left argument is simply the next new list to append "from
-- behind." foldr goes from right to left. foldl goes from left to right.
-- (xs1 ++ xs2) ++ xs3 == xs1 ++ (xs2 ++ xs3) - associativity
-- The foldl version "uses" the left side of this equation.
-- The foldr version "uses" the right side of this equation.
-- This exercise wasn't marked "hard" for the code - it's marked hard b/c
-- of the reasoning involved.
-- On a more general note, note that when the binary operation is
-- associative, foldl and foldr produce the same final result. However,
-- they may not produce the same runtime - and that wholly depends on the
-- runtime of the binary operation.

-- Ex 3.16
addOne :: [Integer] -> [Integer]
addOne = foldr f []
    where f x acc = (x + 1) : acc 
-- It seems that when you want to maintain the structure of the list, foldr
-- is a more natural solution.

-- Ex 3.18
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr f1 [] xs
    where f1 x acc = (f x) : acc

-- Ex 3.19
filter' :: (a -> Bool) -> [a] -> [a]
filter' pred = foldr f []
    where f x acc
            | pred x = x : acc
            | otherwise = acc

-- Ex 3.20
flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f = flatten . (map f)

-- Ex 3.21
-- Use flatMap to implement filter
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' pred = flatMap f
    where f x
            | pred x = [x]
            | otherwise = []

-- Ex 3.22
addLists :: [Integer] -> [Integer] -> [Integer]
addLists [] zs2 = []
addLists zs1 [] = []
addLists (z1:zs1) (z2:zs2) = (z1 + z2) : addLists zs1 zs2

-- Ex 3.23
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] xs2 = []
zipWith' _ xs1 [] = []
zipWith' f (x1:xs1) (x2:xs2) = (f x1 x2) : zipWith' f xs1 xs2

-- Ex 3.24
-- Hmmm...several ways to do this. We can generate the set of all subsequences of the
-- first list, then check if the second list is in that set.
-- We could also maintain a "window" of the same length as the second list,
-- and use it to scan through the first list. 
hasSubsequence :: Eq a => [a] -> [a] -> Bool
hasSubsequence xs1 xs2 = undefined --elem xs2 xs1'
    -- xs1' is xs1 split into lists of length xs2
    where xs1' = undefined --foldl 

-- In case we need it:
subsequences :: [a] -> [[a]]
subsequences [] = []
subsequences list@(x:xs) = (go 1) ++ (subsequences xs) 
    where go n
            | n > length list = []
            | otherwise = (take n list) : (go (n+1))
-- [1,2,3,4] -> [[1], [1,2], [1,2,3], [1,2,3,4], [2], [2,3], [2,3,4], [3],
-- [3,4], [4]]

-- Trees!
data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
            deriving Show

exampleTree = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)

size :: Tree a -> Integer
size (Leaf _) = 1
size (Branch left right) = 1 + (size left) + (size right)

maximum' :: Ord a => Tree a -> a
maximum' (Leaf x) = x
maximum' (Branch left right) = max (maximum' left) (maximum' right)

depth :: Tree a -> Integer
depth (Leaf x) = 0
depth (Branch left right) = max (depth left + 1) (depth right + 1)

mapT :: (a -> b) -> Tree a -> Tree b
mapT f (Leaf x) = Leaf (f x)
mapT f (Branch left right) = Branch (mapT f left) (mapT f right)

foldT :: (b -> b -> b) -> (a -> b) -> Tree a -> b
foldT _ l (Leaf a) = l a
foldT b l (Branch left right) = b (foldT b l left) (foldT b l right)

size' :: Tree a -> Integer
size' = foldT b l
    where b accLeft accRight = 1 + accLeft + accRight
          l = const 1

maximum'' :: Ord a => Tree a -> a
maximum'' = foldT max id

depth' :: Tree a -> Integer
depth' = foldT b l
    where b accLeft accRight = max (accLeft + 1) (accRight + 1)
          l = const 0

mapT' :: (a -> b) -> Tree a -> Tree b
mapT' f = foldT Branch (Leaf . f)
