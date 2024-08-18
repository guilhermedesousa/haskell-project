module Main (main) where

import Utils
import Board
import Player
import Piece
import Moviments
import Checkmate

main :: IO ()
main = do
  let initialBoard = createInitialBoard
  playShogi A initialBoard -- jogador A começa jogando

playShogi :: Player -> Board -> IO ()
playShogi curPlayer board = do
  putStrLn $ show curPlayer ++ " está jogando...\n"
  printBoard board

  let isInCheck = isKingInCheck curPlayer board
  putStrLn $ "O rei de " ++ show curPlayer ++ (if isInCheck then " está em cheque!" else " não está em cheque.")

  let isInCheckMate = isCheckmate curPlayer board
  putStrLn $ "O rei de " ++ show curPlayer ++ (if isInCheckMate then " está em cheque mate!" else " não está em cheque mate.")

  -- let escapes = findEscapeMove curPlayer board
  -- putStrLn "Posições de escape:"
  -- print escapes

  -- TODO: verificar estado do jogo

  handlePlayerInput curPlayer board

handlePlayerInput :: Player -> Board -> IO ()
handlePlayerInput player board = do
  putStrLn "\nInsira uma posição de origem e destino (ex: 3a 4a) ou 'desistir' para encerrar:"
  input <- getLine

  if input == "desistir"
    then putStrLn "Encerrando o programa."
    else do
      let positions = words input
      if length positions /= 2
        then do
          putStrLn "Formato inválido. Use o formato '3a 4a'."
          handlePlayerInput player board  -- Permite tentar novamente
        else do
          let [srcPos, destPos] = positions
          let (srcRow, srcCol) = parsePosition srcPos
          let (destRow, destCol) = parsePosition destPos

          if not (isValidPosition (srcRow, srcCol)) || not (isValidPosition (destRow, destCol))
            then do
              putStrLn "Posição fora dos limites do tabuleiro. Tente novamente."
              handlePlayerInput player board  -- Permite tentar novamente
            else do
              let pieceAtSrc = getPieceFromPosition (srcRow, srcCol) board
              let pieceAtDest = getPieceFromPosition (destRow, destCol) board

              case pieceAtSrc of
                Nothing -> do
                  putStrLn $ "Não há peça na posição " ++ srcPos
                  handlePlayerInput player board  -- Permite tentar novamente
                -- Quando tenta jogar na vez do outro jogador
                Just srcPiece | player /= getPlayer srcPiece -> do
                  putStrLn $ "Movimento inválido: É a vez do jogador " ++ show player
                  handlePlayerInput player board -- Permite tentar novamente
                Just piece -> do
                  case pieceAtDest of  
                    Just destPiece | getPlayer piece == getPlayer destPiece -> do
                      putStrLn "Movimento inválido: Não pode capturar uma peça do mesmo time."
                      handlePlayerInput player board  -- Permite tentar novamente
                    _ -> do
                      let updatedBoard = movePiece (srcRow, srcCol) (destRow, destCol) board
                      case updatedBoard of
                        Nothing -> do
                          putStrLn "Movimento inválido."
                          handlePlayerInput player board  -- Permite tentar novamente
                        Just updatedBoard -> do
                          putStrLn $ "\nPeça na posição " ++ srcPos ++ ": " ++ show piece ++ "\n"
                          let nextPlayer = if player == A then B else A
                          playShogi nextPlayer updatedBoard  -- Chama a função para jogar novamente