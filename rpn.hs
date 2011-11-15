-- LearnYouAHaskell.com has an implementation jus likethis
rpn :: (Num a, Read a, Fractional a, Floating a) => String -> [a]
rpn = foldl operations [] . words 
  
operations :: (Num a, Read a, Fractional a, Floating a) => [a] -> String -> [a]
operations (x:y:xs) "+"  = (y+x):xs
operations (x:y:xs) "-"  = (y-x):xs
operations (x:y:xs) "*"  = (y*x):xs
operations (x:y:xs) "/"  = (y/x):xs
operations (x:y:xs) "^"  = (y ** x):xs
operations xs number = ((read number):xs)

main = do
  line <- getLine
  putStrLn $ show $ rpn line