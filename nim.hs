import Data.Char

-- board utilities : 
type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished:: Board ->Bool
finished b = all (==0) b -- verifier les element de la liste si ils sont egaux a 0

valid :: Board -> Int ->Int -> Bool
valid b row num = b !! (row-1) >= num

move:: Board ->Int ->Int -> Board
move b row num= [adjust r n | (r,n) <- zip [1..5] b]
        where 
            adjust r n = if r ==row then n-num else n

 -- IO utilities
newline :: IO()
newline  = putChar '\n'


stars ::   Int -> String
stars n = if n == 0 then [] else '*': stars (n-1)
-- stars n = concat (replicate n '*')