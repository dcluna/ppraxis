import Control.Applicative

-- if we want to get the indexes, use this function
-- use it like this: getEquilibriumIndexes <$> equilibrium list
-- you may need to import Control.Applicative before doing this, though
getEquilibriumIndexes :: (Num a) => [([a], [a])] -> [Int]
getEquilibriumIndexes list = map (\(prev,next) -> length prev) list

-- finds equilibrated arrays
equilibrium :: (Num a) => [a] -> Maybe [([a], [a])]
equilibrium [] = Nothing
equilibrium list  
  | length list - 1 < 1 = Nothing
  | otherwise = let indexes = [1..(length list - 1)]
                    splitFunctions = map (splitAt) indexes
                    splits = sequence splitFunctions list
                in Just $ filter (\(prev,next) -> sum prev == sum next) splits