-- Tp 2 haskell 
-- Q1
alterne :: [a] -> [a]
alterne [] = []
alterne (x:xs) = if null xs then [x] else x : alterne (tail xs)

-- Q2 
combine :: (a -> b -> c) -> [a] -> [b] -> [c]
combine f [] [] = []
combine f _ [] = []
combine f [] _ = []
combine f (x:xs) (y:ys) = f x y : combine f xs ys 

--Triangle de pascal 
--Q1 
pasPascal :: [Integer] -> [Integer]
pasPascal [1] = [1,1]
pasPascal xs = zipWith (+) (0:xs) (xs++[0])

--Q2 
pascal :: [[Integer]]
pascal = iterate  pasPascal [1] 