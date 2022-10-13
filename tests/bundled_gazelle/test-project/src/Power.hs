module Power where

pow :: Int -> Int -> Int
pow _ 0 = 1
pow a n = a * pow a (n - 1)

karatsuba :: Int -> Int -> Int
karatsuba _ 0 = 1
karatsuba a n
  | even n =
      let sqr = karatsuba a (div n 2) in
      sqr * sqr
  | otherwise =
      let sqr = karatsuba a (div (n - 1) 2) in
      a * sqr * sqr
