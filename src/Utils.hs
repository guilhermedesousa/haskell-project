module Utils where

import Data.Char (digitToInt, isDigit)
import Text.Read (readMaybe)

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
colToInt _   = -1 -- para valores inválidos

parsePosition :: String -> Maybe Position
parsePosition strPos
    | length strPos /= 2  = Nothing
    | not (isDigit xChar) = Nothing
    | x < 1 || x > 9      = Nothing
    | y < 0 || y > 8      = Nothing
    | otherwise           = Just (x - 1, y)
    where
        xChar = strPos !! 0
        x = digitToInt xChar
        yChar = strPos !! 1
        y = colToInt yChar

strToInt :: String -> Maybe Int
strToInt str = readMaybe str