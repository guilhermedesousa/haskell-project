module Moviments where

import Piece hiding (isPromoted)
import Board
import Utils
import Player

movePiece :: Position -> Position -> Board -> Maybe Board
movePiece fromPos toPos board =
    let maybePiece = getPieceFromPosition fromPos board
        newBoard = placePiece fromPos Nothing board
    in case maybePiece of
        Just (Piece pieceType player isPromoted) -> 
            if pieceType == Lanca && tryLanceMove fromPos toPos player board isPromoted then do
                let newPiece = if not isPromoted && isPromotionZone player toPos then (Piece Lanca player True) else (Piece Lanca player isPromoted)
                Just (placePiece toPos (Just newPiece) newBoard)
            else if pieceType == Cavalo && tryHorseMove fromPos toPos player board isPromoted then do
                let newPiece = if not isPromoted && isPromotionZone player toPos then (Piece Cavalo player True) else (Piece Cavalo player isPromoted)
                Just (placePiece toPos (Just newPiece) newBoard)
            else if pieceType == General_Prata && trySilverMove fromPos toPos player board isPromoted then do
                let newPiece = if not isPromoted && isPromotionZone player toPos then (Piece General_Prata player True) else (Piece General_Prata player isPromoted)
                Just (placePiece toPos (Just newPiece) newBoard)
            else if pieceType == General_Ouro && tryGoldMove fromPos toPos player board then do
                let newPiece = if not isPromoted && isPromotionZone player toPos then (Piece General_Ouro player True) else (Piece General_Ouro player isPromoted)
                Just (placePiece toPos (Just newPiece) newBoard)
            else if pieceType == Rei && tryKingMove fromPos toPos player board then do
                let newPiece = if not isPromoted && isPromotionZone player toPos then (Piece Rei player True) else (Piece Rei player isPromoted)
                Just (placePiece toPos (Just newPiece) newBoard)
            else if pieceType == Bispo && tryBishopMove fromPos toPos player board isPromoted then do
                let newPiece = if not isPromoted && isPromotionZone player toPos then (Piece Bispo player True) else (Piece Bispo player isPromoted)
                Just (placePiece toPos (Just newPiece) newBoard)
            else if pieceType == Torre && tryTowerMove fromPos toPos player board isPromoted then do
                let newPiece = if not isPromoted && isPromotionZone player toPos then (Piece Torre player True) else (Piece Torre player isPromoted)
                Just (placePiece toPos (Just newPiece) newBoard)
            else if pieceType == Peao && tryPawnMove fromPos toPos player board isPromoted then do
                let newPiece = if not isPromoted && isPromotionZone player toPos then (Piece Peao player True) else (Piece Peao player isPromoted)
                Just (placePiece toPos (Just newPiece) newBoard)
            else
                Nothing
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

tryPawnMove :: Position -> Position -> Player -> Board -> Bool -> Bool
tryPawnMove (srcRow, srcCol) (desRow, desCol) player board isPromoted =
    if isPromoted then tryGoldMove (srcRow, srcCol) (desRow, desCol) player board else
        let deltaX = desRow - srcRow
            deltaY = desCol - srcCol
            pieceAtDest = getPieceFromPosition (desRow, desCol) board
            isForwardMove = abs deltaX == 1
            isCapture = abs deltaY == 0
            validMove step = isForwardMove && deltaX == step && isCapture || isCapture && deltaX == step && case pieceAtDest of
                Just _  -> True
                Nothing -> False
        in case player of
            A -> validMove 1
            B -> validMove (-1)

tryLanceMove :: Position -> Position -> Player -> Board -> Bool -> Bool
tryLanceMove (srcRow, srcCol) (desRow, desCol) player board isPromoted
    | isPromoted = tryGoldMove (srcRow, srcCol) (desRow, desCol) player board
    | srcCol /= desCol = False  -- Movimento não é horizontal
    | otherwise = validMove && pathClear
  where
    positions = [(row, srcCol) | row <- [min srcRow desRow + 1 .. max srcRow desRow - 1]]
    pathClear = all (\pos -> case getPieceFromPosition pos board of
                              Just _  -> False
                              Nothing -> True) positions
    validMove = case player of
        A -> desRow > srcRow
        B  -> desCol < srcRow

tryKingMove :: Position -> Position -> Player -> Board -> Bool
tryKingMove (srcRow, srcCol) (desRow, desCol) _ _ =
    let deltaX = abs (desRow - srcRow)
        deltaY = abs (desCol - srcCol)
    in deltaX <= 1 && deltaY <= 1

-- Função para verificar se o movimento de um bispo é válido
tryBishopMove :: Position -> Position -> Player -> Board -> Bool -> Bool
tryBishopMove (srcRow, srcCol) (desRow, desCol) player board isPromoted =
    let deltaX = abs (desRow - srcRow)
        deltaY = abs (desCol - srcCol)
        validMove = (deltaX == deltaY) || (isPromoted && validHorVer)
        -- Calcular as posições intermediárias
        positions = if validMove
                    then zip (range srcRow desRow) (range srcCol desCol)
                    else []
        noObstructions = all (\(x, y) -> getPieceFromPosition (x, y) board == Nothing) positions
        -- Valida se pode ir pra frente ou para os lados caso seja promovida
        validHorVer = (deltaX == 1 || deltaY == 1)
    in validMove && noObstructions
  where
    -- Função auxiliar para gerar uma lista de inteiros entre dois valores
    range start end = if start < end
                      then [start + 1 .. end - 1]
                      else [start - 1, start - 2 .. end + 1]

tryTowerMove :: Position -> Position -> Player -> Board -> Bool -> Bool
tryTowerMove (srcRow, srcCol) (desRow, desCol) player board isPromoted =
    let -- Verifica se o movimento é horizontal ou vertical
        validMove = (srcRow == desRow || srcCol == desCol) || (isPromoted && isDiagonalMove)
        -- Função auxiliar para calcular as posições intermediárias para movimentos horizontais ou verticais
        positions = if validMove
                    then if srcRow == desRow
                         then zip (repeat srcRow) (range srcCol desCol)
                         else zip (range srcRow desRow) (repeat srcCol)
                    else []
        noObstructions = all (\(x, y) -> getPieceFromPosition (x, y) board == Nothing) positions
        -- Verifica se o movimento é válido na diagonal caso seja uma peça promovida
        deltaX = desRow - srcRow
        deltaY = desCol - srcCol
        isDiagonalMove = (abs deltaX == 1 && abs deltaY == 1)
    in validMove && noObstructions
  where
    -- Função auxiliar para gerar uma lista de inteiros entre dois valores
    range start end = if start < end
                      then [start + 1 .. end - 1]
                      else [start - 1, start - 2 .. end + 1]

-- Função para verificar se o movimento de um cavalo é válido
tryHorseMove :: Position -> Position -> Player -> Board -> Bool -> Bool
tryHorseMove (srcRow, srcCol) (desRow, desCol) player board isPromoted =
    if isPromoted then tryGoldMove (srcRow, srcCol) (desRow, desCol) player board else
        let deltaX = desRow - srcRow
            deltaY = abs (desCol - srcCol)
            adjustedDeltaX = case player of
                            A -> 2
                            B -> -2
        in (deltaX == adjustedDeltaX && deltaY == 1)

tryGoldMove :: Position -> Position -> Player -> Board -> Bool
tryGoldMove (srcRow, srcCol) (desRow, desCol) player board =
    let deltaX = desRow - srcRow
        deltaY = desCol - srcCol
        -- Verificar se o movimento é válido para o Rei
        isKingMove = deltaX <= 1 && deltaY <= 1 && deltaX >= -1 && deltaY >= -1
        -- Verificar se o movimento é um movimento de Cavalo com base no jogador
        isDiagonalMove = (abs deltaX == 1 && abs deltaY == 1)
        -- Ajustar movimento do cavalo baseado no jogador
        adjustedDiagonalMove = case player of
                             A -> isDiagonalMove && (deltaX == -1 && abs deltaY == 1)
                             B -> isDiagonalMove && (deltaX == 1 && abs deltaY == 1)
    in isKingMove && not adjustedDiagonalMove

trySilverMove :: Position -> Position -> Player -> Board -> Bool -> Bool
trySilverMove (srcRow, srcCol) (desRow, desCol) player board isPromoted =
    if isPromoted then tryGoldMove (srcRow, srcCol) (desRow, desCol) player board else
        let deltaX = desRow - srcRow
            deltaY = desCol - srcCol
            -- Verificar se o movimento é um movimento de Cavalo com base no jogador
            isDiagonalMove = (abs deltaX == 1 && abs deltaY == 1)
            -- Ajustar movimento do cavalo baseado no jogador
            isForwardMove = case player of
                                A -> deltaX == 1
                                B -> deltaX == -1
        in isDiagonalMove || isForwardMove