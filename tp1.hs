--Q1
sommeDeXaY x y = sum [x..y]

-- Q2
somme ns = sum ns

-- Q3
-- redifinition de la fonction last :
lastBis []=error "la liste est vide "
lastBis xs= head (reverse xs)
lastBis2 xs= xs !! (length xs - 1)

-- redifinition de la fonction init :
initbis [] = error "la liste est vide "
initbis xs = reverse(tail (reverse xs))

--Q4 
-- !! avec filtrage de motif
select [] n = error "la liste est vide " 
select (x:xs) n = if n > length xs + 1 then error "n out of bounds"
                        else if n == 0 then x 
                               else select(xs)(n-1)


--plusplusbis xs xss = (xs:xss)


concatrec :: [[a]]->[a]
concatrec [] = []
concatrec (xs:xss) = xs ++ concatrec xss                                  

map2::(a -> b) -> [a] -> [b]
map2 f [] = []
map2 f (x:xs) = f x : map2 f xs

--x = (!!) [1,2,3]

--Q8
length2 :: [a]->Int
length2 [] = 0
length2 xs = somme ( map (\a -> 1) xs )

-- Q9
funRec :: (a->a) -> a -> Int -> [a]
funRec f x 0 =[]
funRec f x n =  x : funRec (f) (f (x)) (n-1)

-- en utilisant iterate 
funIt :: (a->a) -> a -> Int -> [a]
funIt f x 0 =[]
funIt f x n = take n (iterate f x)

-- Q10
funfinalrec :: Int->[Int]
funfinalrec 0 = []
funfinalrec n = funRec (+1) 0 n

funfinalIt :: Int->[Int]
funfinalIt 0 = []
funfinalIt n = funIt (+1) 0 n