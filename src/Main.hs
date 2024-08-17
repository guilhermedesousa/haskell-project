module Main (main) where

import Data.Maybe (catMaybes)
import Utils
import Board
import Player
import Piece
import Moviments
import Capture

main :: IO ()
main = do
  let initialBoard = createInitialBoard
  let capturedPieces = ([], []) -- zero peças capturadas
  
  playShogi A initialBoard capturedPieces -- jogador A começa

playShogi :: Player -> Board -> CapturedPieces -> IO ()
playShogi curPlayer board capturedPieces = do
  putStrLn $ show curPlayer ++ " está jogando...\n"
  let pieces = if curPlayer == A then fst capturedPieces else snd capturedPieces
  putStrLn $ "Peças capturadas pelo jogador " ++ show curPlayer ++ ": "
  printCapturedPieces pieces

  printBoard board

  -- TODO: verificar estado do jogo

  handlePlayerInput curPlayer capturedPieces board

handlePlayerInput :: Player -> CapturedPieces -> Board -> IO ()
handlePlayerInput player capturedPieces board = do
  putStrLn "\nInsira uma posição de origem e destino (ex: 3a 4a), 'repor' ou 'sair' para encerrar:"
  input <- getLine

  if input == "sair"
    then putStrLn "Encerrando o programa."
  else if input == "repor" then do
    handlePieceReplacement player capturedPieces board
  else do
      let positions = words input
      if length positions /= 2
        then do
          putStrLn "Formato inválido. Use o formato '3a 4a'."
          handlePlayerInput player capturedPieces board  -- Permite tentar novamente
        else do
          let [srcPos, destPos] = positions

          case parsePosition srcPos of
            Nothing -> do
              putStrLn "Movimento inválido."
              handlePlayerInput player capturedPieces board  -- Permite tentar novamente
            Just (srcRow, srcCol) -> do
              case parsePosition destPos of
                Nothing -> do
                  putStrLn "Movimento inválido."
                  handlePlayerInput player capturedPieces board  -- Permite tentar novamente
                Just (destRow, destCol) -> do
                  let pieceAtSrc = getPieceFromPosition (srcRow, srcCol) board
                  let pieceAtDest = getPieceFromPosition (destRow, destCol) board

                  case pieceAtSrc of
                    Nothing -> do
                      putStrLn $ "Não há peça na posição " ++ srcPos
                      handlePlayerInput player capturedPieces board  -- Permite tentar novamente
                    -- Quando tenta jogar na vez do outro jogador
                    Just srcPiece | player /= getPlayer srcPiece -> do
                      putStrLn $ "Movimento inválido: É a vez do jogador " ++ show player
                      handlePlayerInput player capturedPieces board -- Permite tentar novamente
                    Just piece -> do
                      case pieceAtDest of  
                        Just destPiece | getPlayer piece == getPlayer destPiece -> do
                          putStrLn "Movimento inválido: Não pode capturar uma peça do mesmo time."
                          handlePlayerInput player capturedPieces board  -- Permite tentar novamente
                        _ -> do
                          let updatedBoard = movePiece (srcRow, srcCol) (destRow, destCol) board
                          let pieceAtDest = getPieceFromPosition (destRow, destCol) board
                          let updatedCapturedPieces = addCapturedPiece capturedPieces pieceAtDest
                          case updatedBoard of
                            Nothing -> do
                              putStrLn "Movimento inválido."
                              handlePlayerInput player capturedPieces board  -- Permite tentar novamente
                            Just updatedBoard -> do
                              putStrLn $ "\nPeça na posição " ++ srcPos ++ ": " ++ show (getType piece) ++ "\n"
                              let nextPlayer = if player == A then B else A
                              playShogi nextPlayer updatedBoard updatedCapturedPieces  -- Chama a função para jogar novamente

handlePieceReplacement :: Player -> CapturedPieces -> Board -> IO ()
handlePieceReplacement player capturedPieces board = do
  let pieces = if player == A then fst capturedPieces else snd capturedPieces
  let onlyPieces = catMaybes pieces
  let countCaptured = length onlyPieces
  if countCaptured == 0
    then do
      putStrLn "Não existem peças para repor."
      handlePlayerInput player capturedPieces board  -- Permite tentar novamente
    else do
      putStrLn "\nInforme o número da peça que queira repor + posição de destino (ex: 1 4a), 'mover' ou 'sair' para encerrar:"
      input <- getLine

      if input == "mover"
        then do
          handlePlayerInput player capturedPieces board -- Permite mover uma peça
      else if input == "sair"
        then putStrLn "Encerrando o programa."
      else do
        let playerChoices = words input
        if length playerChoices /= 2
          then do
            putStrLn "Formato inválido. Use o formato '1 4a'."
            handlePieceReplacement player capturedPieces board  -- Permite tentar novamente
          else do
            let [playerChoice, destPos] = playerChoices

            case parsePieceChoice countCaptured playerChoice of
              Nothing -> do
                putStrLn "Escolha inválida."
                handlePieceReplacement player capturedPieces board  -- Permite tentar novamente
              Just pieceNumber -> do
                case parsePosition destPos of
                  Nothing -> do
                    putStrLn "Movimento inválido."
                    handlePieceReplacement player capturedPieces board  -- Permite tentar novamente
                  Just (destRow, destCol) -> do
                    let pieceToReplace = onlyPieces !! (pieceNumber - 1)
                    let pieceAtDest = getPieceFromPosition (destRow, destCol) board

                    case pieceAtDest of  
                      Just destPiece -> do
                        putStrLn "Movimento inválido: Peças são reposicionadas apenas em casas vazias."
                        handlePieceReplacement player capturedPieces board  -- Permite tentar novamente
                      _ -> do
                        -- validReplacement (Just pieceToReplace) (destRow, destCol) board
                        case (validReplacement (Just pieceToReplace) (destRow, destCol) board) of
                          Nothing -> do
                            putStrLn "Reposição inválida."
                            handlePieceReplacement player capturedPieces board  -- Permite tentar novamente
                          Just False -> do
                            putStrLn "Reposição inválida."
                            handlePieceReplacement player capturedPieces board  -- Permite tentar novamente
                          _ -> do
                            -- TODO: validate pawn drop (column, row, checkmate)
                            -- let isValidReplacement = validReplacement pieceToReplace (destRow, destCol) board
                            -- case isValidReplacement of
                              
                            -- TODO: validate lance drop (row)
                            -- TODO: validate horse drop (row)

                            let updatedCapturedPieces = removeCapturedPiece capturedPieces pieceNumber pieceToReplace
                            let updatedBoard = dropPiece pieceToReplace (destRow, destCol) board

                            case updatedBoard of
                              Nothing -> do
                                putStrLn "Movimento inválido."
                                handlePieceReplacement player capturedPieces board  -- Permite tentar novamente
                              Just updatedBoard -> do
                                -- putStrLn $ "\nPeça na posição " ++ srcPos ++ ": " ++ show (getType piece) ++ "\n"
                                let nextPlayer = if player == A then B else A
                                playShogi nextPlayer updatedBoard updatedCapturedPieces  -- Chama a função para jogar novamente
                            -- let pieceAtDest = getPieceFromPosition (destRow, destCol) board
                            -- let updatedCapturedPieces = addCapturedPiece capturedPieces pieceAtDest
                            -- case updatedBoard of
                            --   Nothing -> do
                            --     putStrLn "Movimento inválido."
                            --     handlePlayerInput player capturedPieces board  -- Permite tentar novamente
                            --   Just updatedBoard -> do
                            --     putStrLn $ "\nPeça na posição " ++ srcPos ++ ": " ++ show (getType piece) ++ "\n"
                            --     let nextPlayer = if player == A then B else A
                            --     playShogi nextPlayer updatedBoard updatedCapturedPieces  -- Chama a função para jogar novamente