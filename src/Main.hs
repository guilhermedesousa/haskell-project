module Main (main) where

import Board
import Player
import Piece

main :: IO ()
main = do
  let initialBoard = createInitialBoard
  let dummyCell = createCell A Peao
  let newBoard = updateCell (2, 2) (3, 2) dummyCell initialBoard
  printBoard initialBoard
  printBoard newBoard