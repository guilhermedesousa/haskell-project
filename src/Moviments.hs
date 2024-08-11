module Moviments where

import Piece
import Board
import Utils
import Player

-- Função principal para mover uma peça
-- movePiece :: Position -> Position -> Board -> Maybe Board
-- movePiece fromPos toPos board =
--     let maybePiece = getPieceFromPosition fromPos board
--         newBoard = placePiece fromPos Nothing board
--     in case maybePiece of
--         Just Rei ->
--             if isValidReiMove fromPos toPos
--                 then Just (placePiece toPos (Just Rei) newBoard)
--             else Nothing
--         Just Peao ->
--             if isValidPeaoMove fromPos toPos board
--                 then Just (placePiece toPos (Just Peao) newBoard)
--             else Nothing
--         Just Cavalo ->
--             if isValidCavaloMove fromPos toPos board
--                 then Just (placePiece toPos (Just Cavalo) newBoard)
--             else Nothing
--         Just Bispo ->
--             if isValidBispoMove fromPos toPos board
--                 then Just (placePiece toPos (Just Bispo) newBoard)
--             else Nothing
--         Just Torre ->
--             if isValidTorreMove fromPos toPos board
--                 then Just (placePiece toPos (Just Torre) newBoard)
--             else Nothing
--         Just General_Ouro ->
--             if isValidGeneral_OuroMove fromPos toPos board
--                 then Just (placePiece toPos (Just General_Ouro) newBoard)
--             else Nothing
--         Nothing -> Nothing

movePiece :: Position -> Position -> Board -> Maybe Board
movePiece fromPos toPos board =
    let maybePiece = getPieceFromPosition fromPos board
        newBoard = placePiece fromPos Nothing board
    in case maybePiece of
        Just (Piece Lanca player isPromoted) -> 
            if isValidLanceMove fromPos toPos player board
                then Just (placePiece toPos (Just (Piece Lanca player isPromoted)) newBoard)
            else Nothing
        Just (Piece Cavalo player isPromoted) -> Just (placePiece toPos (Just (Piece Cavalo player isPromoted)) newBoard)
        Just (Piece General_Prata player isPromoted) -> Just (placePiece toPos (Just (Piece General_Prata player isPromoted)) newBoard)
        Just (Piece General_Ouro player isPromoted) -> Just (placePiece toPos (Just (Piece General_Ouro player isPromoted)) newBoard)
        Just (Piece Rei player isPromoted) -> Just (placePiece toPos (Just (Piece Rei player isPromoted)) newBoard)
        Just (Piece Bispo player isPromoted) -> Just (placePiece toPos (Just (Piece Bispo player isPromoted)) newBoard)
        Just (Piece Torre player isPromoted) -> Just (placePiece toPos (Just (Piece Torre player isPromoted)) newBoard)
        Just (Piece Peao player isPromoted) -> 
            if isValidPawnMove fromPos toPos player board
                then Just (placePiece toPos (Just (Piece Peao player isPromoted)) newBoard)
            else Nothing
        Nothing -> Nothing

placePiece :: Position -> Maybe Piece -> Board -> Board
placePiece (row, col) maybePiece board =
    let -- Atualiza a linha no índice y
        updatedRow = take col (board !! row) ++ [maybePiece] ++ drop (col + 1) (board !! row)
        -- Atualiza o tabuleiro substituindo a linha y com a linha atualizada
        newBoard = take row board ++ [updatedRow] ++ drop (row + 1) board
    in newBoard

-- Função para obter a peça em uma determinada posição do tabuleiro
getPieceFromPosition :: Position -> Board -> Cell
getPieceFromPosition (row, col) board =
    -- Verifica se a posição está dentro dos limites do tabuleiro
    if row >= 0 && row < length board
       && col >= 0 && col < length (board !! row)
    then (board !! row) !! col  -- Acessa a célula na posição (row, col)
    else Nothing  -- Se a posição estiver fora dos limites

isValidPawnMove :: Position -> Position -> Player -> Board -> Bool
isValidPawnMove (srcRow, srcCol) (desRow, desCol) player board =
    let deltaX = desRow - srcRow
        deltaY = desCol - srcCol
        pieceAtDest = getPieceFromPosition (desRow, desCol) board
        isForwardMove = abs deltaX == 1
        isCapture = abs deltaY == 0
        validMove step = isForwardMove && deltaX == step || isCapture && deltaX == step && case pieceAtDest of
            Just _  -> True
            Nothing -> False
    in case player of
        A -> validMove 1
        B -> validMove (-1)

isValidLanceMove :: Position -> Position -> Player -> Board -> Bool
isValidLanceMove (srcRow, srcCol) (desRow, desCol) player board
    | srcCol /= desCol = False  -- Movimento não é vertical
    | otherwise = validMove && pathClear
  where
    positions = [(row, srcCol) | row <- [min srcRow desRow + 1 .. max srcRow desRow - 1]]
    pathClear = all (\pos -> case getPieceFromPosition pos board of
                              Just _  -> False
                              Nothing -> True) positions
    validMove = case player of
        A -> desRow > srcRow
        B  -> desCol < srcRow