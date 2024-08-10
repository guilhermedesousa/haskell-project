module Main (main) where

import Board

main :: IO ()
main = do
  let board = createInitialBoard
  printBoard board
