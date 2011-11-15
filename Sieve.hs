import Data.List
primes :: Int -> [Int]
primes n 
  | n <= 1 = []
  | n == 2 = [2]
  | otherwise = 2:sieve [3,5..n] -- optimization: except for 2, only odd numbers are eligible for primality

sieve :: [Int] -> [Int]
sieve [] = []
--sieve (n:ns) = n:(sieve $ filterDivisibleBy n ns) -- should prolly use '\\' here...
sieve (n:ns) = n:(sieve $ optimizedFilter [n^2,(n+1)*n..last ns] ns) -- optimization: start removing elements from n-squared, because the predecessors have already been processed by previous iterations
--sieve (n:ns) = n:(sieve (ns \\ [n^2,(n+1)*n..last ns])) -- optimization: start removing elements from n-squared, because the predecessors have already been processed by previous iterations

filterDivisibleBy :: Int -> [Int] -> [Int]
filterDivisibleBy n ns = filter (\x -> x `mod` n /= 0) ns

optimizedFilter :: [Int] -> [Int] -> [Int]
optimizedFilter divisors ns = filter (\x -> (x `elem` divisors) == False) ns