module Statistics ( 
  mean
  ) where

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

-- subtract = flip (-)
stddev :: [Double] -> Double
stddev xs = sqrt ( invn * sum (map (^2) (map (subtract (mean xs)) xs)) )
  where invn = 1 / fromIntegral (length xs)