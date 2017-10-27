import Findall
import Integer ( abs )
import Read
import System

main :: IO ()
main = do [strN] <- getArgs
          let n = readInt strN
          values <- getAllValues $ queens n 0 []
          putStrLn $ show $ length values

chooseInt :: Int -> Int
chooseInt n | n > 0 = n-1 ? chooseInt (n-1)

okay :: Int -> Int -> [Int] -> Bool
okay i c []     = True
okay i c (x:xs) = c /= x && (c-x) /= i && (c-x) /= -i && okay (i+1) c xs

queens :: Int -> Int -> [Int] -> [Int]
queens n i l =
       if n == i then
          l
       else
          let c = chooseInt n in
            if okay 1 c l then queens n (i+1) (c : l) else failed
