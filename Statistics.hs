module Statistics ( 
  average
  ) where

average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)