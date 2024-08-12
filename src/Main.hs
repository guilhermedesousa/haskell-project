module Main (main) where

import Utils
import Board
import Player
import Piece
import Moviments

main :: IO ()
main = do
  let initialBoard = createInitialBoard
  playShogi A initialBoard

playShogi :: Player -> Board -> IO ()
playShogi curPlayer board = do
  putStrLn $ show curPlayer ++ " está jogando...\n"
  printBoard board
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
          then do
            putStrLn "Formato inválido. Use o formato '3a 4a'."
            handleUserInput board  -- Permite tentar novamente
          else do
            let [srcPos, destPos] = positions
            let (srcRow, srcCol) = parsePosition srcPos
            let (destRow, destCol) = parsePosition destPos
            let pieceAtSrc = getPieceFromPosition (srcRow, srcCol) board
            let pieceAtDest = getPieceFromPosition (destRow, destCol) board
            case pieceAtSrc of
              Nothing -> do
                putStrLn $ "Não há peça na posição " ++ srcPos
                handleUserInput board  -- Permite tentar novamente
              Just piece -> do
                case pieceAtDest of
                  Just destPiece | getPlayer piece == getPlayer destPiece -> do
                    putStrLn "Movimento inválido: Não pode capturar uma peça do mesmo time."
                    handleUserInput board  -- Permite tentar novamente
                  _ -> do
                    let updatedBoard = movePiece (srcRow, srcCol) (destRow, destCol) board
                    case updatedBoard of
                      Nothing -> do
                        putStrLn "Movimento inválido."
                        handleUserInput board  -- Permite tentar novamente
                      Just updatedBoard -> do
                        putStrLn $ "\nPeça na posição " ++ srcPos ++ ": " ++ show piece
                        printBoard updatedBoard
                        handleUserInput updatedBoard  -- Chama a função novamente para continuar o loop