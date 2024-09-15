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

-- -- Verifica se o rei está em cheque
-- isKingInCheck :: Player -> Board -> ShogiGame Bool
-- isKingInCheck player b = do
--     kingPos <- findKingCoordinate b player
--     let opponentPlayer = opponent player

--     or <$> mapM (\pos -> do
--         piece <- getPieceFromPosition pos
--         case piece of
--             Just (Piece _ piecePlayer _) ->
--                 if piecePlayer == opponentPlayer
--                 then isValidMove pos kingPos
--                 else return False
--             _ -> return False
--         ) allPositions

-- isCheckmate :: Player -> Board -> ShogiGame Bool
-- isCheckmate player b = do
--     isInCheck <- isKingInCheck player b
--     canScape <- canNotScape player
--     return $ isInCheck && canScape

-- canNotScape :: Player -> ShogiGame Bool
-- canNotScape player = do
--     b <- gets board
--     playerPositions <- playerPiecePositions player b
--     possibleBoards <- concat <$> mapM (generateBoards b) playerPositions
--     allM (isKingInCheck player) possibleBoards

-- generateBoards :: Board -> Position -> ShogiGame [Board]
-- generateBoards b fromPos = do
--     validBoards <- mapM (tryMovePieceM fromPos b) allPositions
--     return $ catMaybes validBoards

-- tryMovePieceM :: Position -> Board -> Position -> ShogiGame (Maybe Board)
-- tryMovePieceM fromPos b toPos = do
--     isValid <- isValidPosition toPos
--     if isValid
--         then tryMovePiece fromPos toPos
--     else return Nothing

-- -- Função auxiliar que retorna todas as posições das peças de um jogador no tabuleiro
-- playerPiecePositions :: Player -> Board -> ShogiGame [Position]
-- playerPiecePositions player board = do
--     let positions = [(r, c) | r <- [0..8], c <- [0..8]]
--     filteredPositions <- filterM (\pos -> do
--         piece <- getPieceFromPosition pos
--         case piece of
--             Just p -> return (getPlayer p == player)
--             Nothing -> return False
--         ) positions
--     return filteredPositions

-- -- Função para encontrar a posição do rei do jogador
-- findKingCoordinate :: Board -> Player -> ShogiGame Position
-- findKingCoordinate board player = do
--     let positions = allPositions
--     kingPos <- filterM isKing positions
--     return $ head kingPos
--   where
--     isKing pos = do
--         piece <- getPieceFromPosition pos
--         return $ case piece of
--             Just (Piece Rei piecePlayer _) -> piecePlayer == player
--             _ -> False

-- allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
-- allM p = foldr (\x acc -> p x >>= \result -> if not result then return False else acc) (return True)