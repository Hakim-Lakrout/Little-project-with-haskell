import System.IO

act :: IO (Char,Char)
act = do x <- getChar 
         getChar
         y <-getChar
         return (x,y)


getLine2 :: IO String 
getLine2 = do x <- getChar
              if x == '\n' then 
                return []
              else 
                do xs <- getLine2
                   return (x:xs)






match :: String -> String ->String
match xs ys = [if elem x ys then x else '_'|x<-xs]

play :: String ->IO()
play word = do putStr "?"
               guess <- getLine
               if guess == word then
                    putStrLn "brv"
               else
                    do putStrLn (match word guess)
                       play word

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "the String has "
            putStr (show (length xs))
            putStr "  characters \n"

getCh :: IO Char
getCh = do hSetEcho stdin False
           x<-getChar
           hSetEcho stdin True
           return x 



sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                do putChar x
                   return []
              else
                do putChar '_'
                   xs <- sgetLine
                   return(x:xs)






hangman :: IO()
hangman = do putStrLn " think of a word : "
             word<-sgetLine
             putStrLn "try to guess it : "
             play word

