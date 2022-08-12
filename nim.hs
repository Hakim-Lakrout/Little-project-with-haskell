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
--stars n = if n == 0 then [] else '*': stars (n-1)
stars n = concat (replicate n "* ")

putRow :: Int -> Int ->IO()
putRow row num = do putStr(show row)  -- la fonction show conertit un integer to a string pour l afficher 
                    putStr " : "
                    putStrLn (stars num)


putBoard :: Board ->IO()
putBoard [a,b,c,d,e] = do putRow 1 a 
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x<-getChar
                     newline
                     if isDigit x then 
                        return (digitToInt x) -- cette fonction se trouve dans data.Char pour caster la chaine de caractere en un entier
                     else 
                        do newline
                           putStrLn "invalid chaar"
                           getDigit prompt


-- game 
nim :: IO()
nim = play initial 1 


next :: Int -> Int
next 1 = 2
next 2 = 1

play :: Board -> Int -> IO()
play board player = 
    do newline
       putBoard board
       if finished board then 
            do newline
               putStr " player "
               putStr (show(next player))
               putStrLn " has won"
       else 
            do newline
               putStr "player"
               putStrLn (show(player))
               r<-getDigit "enter a row number: "
               n<-getDigit "enter a stars number : "
               if valid board r n then
                    play (move board r n) (next player)
               else 
                    do putStrLn "ERROR : INVALID MOOO VVVEEE"
                       play board player
