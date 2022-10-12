module Fibo where

import Control.Applicative.Lift ()

fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

betterFibo :: Int -> Int
betterFibo 0 = 0
betterFibo n =
  let aux :: Int -> (Int, Int)
      aux 1 = (1, 0)
      aux n =
        let (fib_moins_1, fib_moins_2) = aux (n - 1) in
        (fib_moins_1 + fib_moins_2, fib_moins_1)
  in
  fst (aux n)

fiboRecTerm :: Int -> Int
fiboRecTerm 0 = 0
fiboRecTerm n =
  let aux :: Int -> (Int, Int) -> Int
      aux 1 (a, _) = a
      aux n (a, b) = aux (n-1) (a + b, a)
  in aux n (1, 0)
