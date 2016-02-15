module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

factorial :: Integer -> Integer
factorial n = go n 1
    where go 1 acc = acc
          go n acc = go (n-1) (acc*n)

fib :: Integer -> Integer
fib 1 = 0
fib 2 = 1
fib n = fib (n-1) + fib (n-2)
