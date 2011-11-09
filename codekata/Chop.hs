module Chop(
  chop
  ) where
chop :: Int -> [Int] -> Int
chop n [x]
  | n == x   = 0
  | otherwise = -1
chop n list
  | n == half         = mid
  | n > half          = let result = (chop n larger)
                        in if result == -1 then -1 --is there a better way to do this?
                           else result + mid
  | otherwise         = (chop n smaller)
    where max = length list
          mid = div max 2
          half = list !! mid
          smaller = take mid list
          larger = drop mid list