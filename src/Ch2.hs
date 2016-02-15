module Ch2
    ( factorial
    , fib
    ) where


factorial :: Integer -> Integer
factorial n = go n 1
    where go 1 acc = acc
          go n acc = go (n-1) (acc*n)

-- Ex. 2.1
fib :: Integer -> Integer
fib 1 = 0
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

-- Ex. 2.2
-- isSorted :: (a -> a -> Bool) -> [a] -> Bool
isSorted :: Ord a => [a] -> Bool
isSorted (x:[]) = True
isSorted (x:xs) 
    | x <= (head xs) = isSorted xs
    | otherwise = False

{-
-
- If a function is polymorphic in some type A, the only operations that can
- be performed on that A are those passed into the functions as arguments
- (or that can be defined in terms of these given operations).
- In some cases, you'll find that the universe of possibilities for a given
- polymorphic type is constrained such that only one implementation is
- possible!
-
-}

-- Ex. 2.3
curry :: ((a,b) -> c) -> (a -> b -> c)
curry f = \a -> \b -> f (a, b)

-- Ex. 2.4
uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f = \(a,b) -> f a b

-- Ex. 2.5
compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g = \a -> g $ f a
