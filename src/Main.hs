module Main (main) where

import Utils
import Board
import Player
import Piece
import Moviments

main :: IO ()
main = do
  let initialBoard = createInitialBoard
  -- let dummyCell = createCell A Peao
  -- let newBoard = updateCell (2, 2) (3, 2) dummyCell initialBoard
  printBoard initialBoard
  -- printBoard newBoard

  handleUserInput initialBoard


handleUserInput :: Board -> IO ()
handleUserInput board = do
    putStrLn "\nInsira uma posição de origem e destino (ex: 3a 4a) ou 'sair' para encerrar:"
    input <- getLine
    if input == "sair"
      then putStrLn "Encerrando o programa."
      else do
        let positions = words input
        if length positions /= 2
          then putStrLn "Formato inválido. Use o formato '3a 4a'."
          else do
            let [srcPos, destPos] = positions
            let (srcRow, srcCol) = parsePosition srcPos
            let (destRow, destCol) = parsePosition destPos
            let pieceAtSrc = getPieceFromPosition (srcRow, srcCol) board
            putStrLn $ "Posição de origem: (" ++ show srcRow ++ ", " ++ show srcCol ++ ")"
            putStrLn $ "Posição de destino: (" ++ show destRow ++ ", " ++ show destCol ++ ")"
            case pieceAtSrc of
              Nothing -> putStrLn $ "Não há peça na posição " ++ srcPos
              Just piece -> do
                let updatedBoard = movePiece (srcRow, srcCol) (destRow, destCol) board
                case updatedBoard of
                  Nothing -> putStrLn "Movimento inválido."
                  Just updatedBoard -> do
                    putStrLn $ "\nPeça na posição " ++ srcPos ++ ": " ++ show piece
                    printBoard updatedBoard
                    handleUserInput updatedBoard  -- Chama a função novamente para continuar o loop

        