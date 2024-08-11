module Utils where

import Data.Char

type Position = (Int, Int)

colToInt :: Char -> Int
colToInt 'a' = 0
colToInt 'b' = 1
colToInt 'c' = 2
colToInt 'd' = 3
colToInt 'e' = 4
colToInt 'f' = 5
colToInt 'g' = 6
colToInt 'h' = 7
colToInt 'i' = 8

parsePosition :: String -> Position
parsePosition strPos = (x, y)
    where
        x = digitToInt (strPos !! 0) - 1
        y = colToInt $ strPos !! 1