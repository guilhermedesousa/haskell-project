-- module Checkmate (isKingInCheck, isCheckmate, opponent) where
module Checkmate where

import Control.Monad.State
import Control.Monad (filterM)
import Piece hiding (isPromoted)
import Data.Maybe (catMaybes, isJust)
import Board
import Utils
import Player
import Moviments

-- Verifica se o rei está em cheque
isKingInCheck :: Player -> Board -> ShogiGame Bool
isKingInCheck player b = do
    kingPos <- findKingCoordinate2 b player
    let opponentPlayer = opponent player

    or <$> mapM (\pos -> do
        piece <- getPieceFromPosition pos
        case piece of
            Just (Piece pieceType piecePlayer isPromoted) -> do
                if piecePlayer == opponentPlayer
                then do
                    moveResult <- tryMovePieceM pos b kingPos
                    -- Apenas imprime o movimento se ele causar check
                    case moveResult of
                        Just updatedBoard -> do
                            return True  -- Movimento válido
                        Nothing -> do
                            return False -- Movimento inválido
                        Nothing -> return False
                else return False
            _ -> return False
        ) allPositions

isCheckmate :: Player -> Board -> ShogiGame Bool
isCheckmate player b = do
    isInCheck <- isKingInCheck player b
    cantScape <- canNotScape player

    return $ isInCheck && cantScape

canNotScape :: Player -> ShogiGame Bool
canNotScape player = do
    b <- gets board
    playerPositions <- playerPiecePositions player b
    possibleBoards <- concat <$> mapM (generateBoards b) playerPositions

    -- Verifica se o rei está em cheque em cada tabuleiro possível e imprime o resultado
    checkResults <- mapM (\board -> do
        isKingInCheck player board
      ) possibleBoards

    return (and checkResults)

generateBoards :: Board -> Position -> ShogiGame [Board]
generateBoards b fromPos = do
    validBoards <- mapM (tryMovePieceM fromPos b) allPositions
    return $ catMaybes validBoards

tryMovePieceM :: Position -> Board -> Position -> StateT GameState IO (Maybe Board)
tryMovePieceM fromPos b toPos = do
    -- Checa se a posição destino é válida
    isValid <- isValidPosition toPos
    if isValid
        then do
            resultBoard <- tryMovePiece2 b fromPos toPos
            case resultBoard of
                Just updatedBoard -> return (Just updatedBoard)  -- Se o movimento foi bem-sucedido, retorna o tabuleiro atualizado
                Nothing           -> return Nothing              -- Caso contrário, retorna Nothing
        else return Nothing  -- Se a posição for inválida, retorne Nothing

findEscapeMove :: Player -> Board -> ShogiGame (Maybe (Position, Position))
findEscapeMove player board = do
    playerPositions <- playerPiecePositions player board
    possibleMoves <- filterM (\(fromPos, toPos) -> do
        newBoard <- tryMovePieceM fromPos board toPos
        case newBoard of
            Just b' -> not <$> isKingInCheck player b'
            Nothing -> return False
      ) [(fromPos, toPos) | fromPos <- playerPositions, toPos <- allPositions]

    return $ case possibleMoves of
        []       -> Nothing
        (move:_) -> Just move

escapePositions :: Player -> Board -> ShogiGame [(Position, Position)]
escapePositions player board = do
    inCheck <- isKingInCheck player board
    if not inCheck
        then return []  -- Se o rei não está em xeque, não há movimentos de escape
        else do
            playerPositions <- playerPiecePositions player board
            filterM (\(fromPos, toPos) -> do
                newBoard <- tryMovePieceM fromPos board toPos
                case newBoard of
                    Just b' -> not <$> isKingInCheck player b'
                    Nothing -> return False
              ) [(fromPos, toPos) | fromPos <- playerPositions, toPos <- allPositions]

-- Função auxiliar que retorna todas as posições das peças de um jogador no tabuleiro
playerPiecePositions :: Player -> Board -> ShogiGame [Position]
playerPiecePositions player board = do
    let positions = [(r, c) | r <- [0..8], c <- [0..8]]
    filteredPositions <- filterM (\pos -> do
        piece <- getPieceFromPosition pos
        case piece of
            Just p -> return (getPlayer p == player)
            Nothing -> return False
        ) positions
    return filteredPositions

-- Função para encontrar a posição do rei do jogador
findKingCoordinate :: Board -> Player -> ShogiGame Position
findKingCoordinate board player = do
    let positions = allPositions
    kingPos <- filterM isKing positions
    return $ head kingPos
  where
    isKing pos = do
        piece <- getPieceFromPosition pos
        return $ case piece of
            Just (Piece Rei piecePlayer _) -> piecePlayer == player
            _ -> False

findKingCoordinate2 :: Board -> Player -> ShogiGame Position
findKingCoordinate2 board player = do
    let positions = allPositions
    kingPos <- filterM isKing positions
    return $ head kingPos
  where
    isKing pos = do
        piece <- getPieceFromPosition2 board pos
        return $ case piece of
            Just (Piece Rei piecePlayer _) -> piecePlayer == player
            _ -> False

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p = foldr (\x acc -> p x >>= \result -> if not result then return False else acc) (return True)