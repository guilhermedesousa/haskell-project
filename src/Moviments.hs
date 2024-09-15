{-# LANGUAGE TupleSections #-}
module Moviments where

import Control.Monad.State
import Piece hiding (isPromoted)
import Board
import Utils
import Player

tryMovePiece :: Position -> Position -> ShogiGame Bool
tryMovePiece fromPos toPos = do
    pieceAtSrc  <- getPieceFromPosition fromPos
    pieceAtDest <- getPieceFromPosition toPos

    case pieceAtSrc of
        Nothing -> return False
        Just (Piece pieceType player isPromoted) ->
            case pieceAtDest of
                Just (Piece _ destPlayer _) | destPlayer == player -> return False
                _ ->
                    case pieceType of
                        Lanca -> do
                            succLanceMove <- tryLanceMove fromPos toPos isPromoted
                            if succLanceMove
                                then do
                                    let newPiece = if not isPromoted && isPromotionZone player toPos then Piece Lanca player True else Piece Lanca player isPromoted
                                    updateBoard fromPos toPos newPiece
                            else
                                return False
                        Cavalo -> do
                            succHorseMove <- tryHorseMove fromPos toPos isPromoted
                            if succHorseMove
                                then do
                                    let newPiece = if not isPromoted && isPromotionZone player toPos then Piece Cavalo player True else Piece Cavalo player isPromoted
                                    updateBoard fromPos toPos newPiece
                            else
                                return False
                        General_Prata -> do
                            succSilverMove <- trySilverMove fromPos toPos isPromoted
                            if succSilverMove
                                then do
                                    let newPiece = if not isPromoted && isPromotionZone player toPos then Piece General_Prata player True else Piece General_Prata player isPromoted
                                    updateBoard fromPos toPos newPiece
                            else
                                return False
                        General_Ouro -> do
                            succGoldMove <- tryGoldMove fromPos toPos
                            if succGoldMove
                                then do
                                    let newPiece = if not isPromoted && isPromotionZone player toPos then Piece General_Ouro player True else Piece General_Ouro player isPromoted
                                    updateBoard fromPos toPos newPiece
                            else
                                return False
                        Rei -> do
                            succKingMove <- tryKingMove fromPos toPos
                            if succKingMove
                                then do
                                    let newPiece = if not isPromoted && isPromotionZone player toPos then Piece Rei player True else Piece Rei player isPromoted
                                    updateBoard fromPos toPos newPiece
                            else
                                return False
                        Bispo -> do
                            succBishopMove <- tryBishopMove fromPos toPos isPromoted
                            if succBishopMove
                                then do
                                    let newPiece = if not isPromoted && isPromotionZone player toPos then Piece Bispo player True else Piece Bispo player isPromoted
                                    updateBoard fromPos toPos newPiece
                            else
                                return False
                        Torre -> do
                            succTowerMove <- tryTowerMove fromPos toPos isPromoted
                            if succTowerMove
                                then do
                                    let newPiece = if not isPromoted && isPromotionZone player toPos then Piece Torre player True else Piece Torre player isPromoted
                                    updateBoard fromPos toPos newPiece
                            else
                                return False
                        Peao -> do
                            succPawnMove <- tryPawnMove fromPos toPos isPromoted
                            if succPawnMove
                                then do
                                    let newPiece = if not isPromoted && isPromotionZone player toPos then Piece Peao player True else Piece Peao player isPromoted
                                    updateBoard fromPos toPos newPiece
                            else
                                return False

isValidMove :: Position -> Position -> ShogiGame Bool
isValidMove fromPos toPos = do
    b <- gets board
    piece <- getPieceFromPosition fromPos
    case piece of
        Nothing -> return False
        Just _  -> tryMovePiece fromPos toPos

placePiece :: Position -> Maybe Piece -> ShogiGame ()
placePiece (row, col) mPiece = do
    b <- gets board
    let updatedBoard = take row b ++ [take col (b !! row) ++ [mPiece] ++ drop (col + 1) (b !! row)] ++ drop (row + 1) b
    modify $ \gs -> gs { board = updatedBoard }

updateBoard :: Position -> Position -> Piece -> ShogiGame Bool
updateBoard fromPos toPos mPiece = do
    placePiece fromPos Nothing
    placePiece toPos (Just mPiece)
    return True

getPieceFromPosition :: Position -> ShogiGame Cell
getPieceFromPosition (row, col) = do
    b <- gets board
    isValid <- isValidPosition (row, col)
    if isValid
        then return $ (b !! row) !! col
        else return Nothing

tryPawnMove :: Position -> Position -> Bool -> ShogiGame Bool
tryPawnMove (srcRow, srcCol) (desRow, desCol) isPromoted = do
    b           <- gets board
    player      <- gets currentPlayer
    pieceAtDest <- getPieceFromPosition (desRow, desCol)

    if isPromoted
    then tryGoldMove (srcRow, srcCol) (desRow, desCol)
    else do
        let deltaX = desRow - srcRow
            deltaY = desCol - srcCol
            isForwardMove = abs deltaX == 1
            isCapture = abs deltaY == 0

            validMove step = isForwardMove && deltaX == step && isCapture || isCapture && deltaX == step && case pieceAtDest of
                Just _  -> True
                Nothing -> False
        return $ case player of
            A -> validMove 1
            B -> validMove (-1)

tryLanceMove :: Position -> Position -> Bool -> ShogiGame Bool
tryLanceMove (srcRow, srcCol) (desRow, desCol) isPromoted = do
    player <- gets currentPlayer

    if isPromoted
        then tryGoldMove (srcRow, srcCol) (desRow, desCol)
    else if srcCol /= desCol
        then return False
    else do
        let positions = [(row, srcCol) | row <- [min srcRow desRow + 1 .. max srcRow desRow - 1]]
        pieces <- mapM getPieceFromPosition positions
        let pathClear = all (== Nothing) pieces
        let validMove = if player == A then desRow > srcRow else desRow < srcRow

        return $ validMove && pathClear

tryKingMove :: Position -> Position -> ShogiGame Bool
tryKingMove (srcRow, srcCol) (desRow, desCol) = do
    let deltaX = abs (desRow - srcRow)
        deltaY = abs (desCol - srcCol)
    return $ deltaX <= 1 && deltaY <= 1

tryBishopMove :: Position -> Position -> Bool -> ShogiGame Bool
tryBishopMove (srcRow, srcCol) (desRow, desCol) isPromoted = do
    let deltaX = abs (desRow - srcRow)
        deltaY = abs (desCol - srcCol)
        -- Valida se pode ir pra frente ou para os lados caso seja promovida
        validHorVer = (deltaX == 1 || deltaY == 1)
        validMove = (deltaX == deltaY) || (isPromoted && validHorVer)
        range start end = if start < end
                          then [start + 1 .. end - 1]
                          else [start - 1, start - 2 .. end + 1]

        -- Calcular as posições intermediárias
        positions = if validMove
                    then zip (range srcRow desRow) (range srcCol desCol)
                    else []
    pieces <- mapM getPieceFromPosition positions
    let noObstructions = all (== Nothing) pieces

    return $ validMove && noObstructions

tryTowerMove :: Position -> Position -> Bool -> ShogiGame Bool
tryTowerMove (srcRow, srcCol) (desRow, desCol) isPromoted = do
    let deltaX = desRow - srcRow
        deltaY = desCol - srcCol

        isDiagonalMove = (abs deltaX == 1 && abs deltaY == 1)
        validMove = (srcRow == desRow || srcCol == desCol) || (isPromoted && isDiagonalMove)

        -- Função auxiliar para calcular as posições intermediárias para movimentos horizontais ou verticais
        range start end = if start < end
                      then [start + 1 .. end - 1]
                      else [start - 1, start - 2 .. end + 1]

        positions = if validMove
                    then if srcRow == desRow
                         then map (srcRow,) (range srcCol desCol)
                         else map (, srcCol) (range srcRow desRow)
                    else []
        
    pieces <- mapM getPieceFromPosition positions
    let noObstructions = all (== Nothing) pieces
        
    return $ validMove && noObstructions

tryHorseMove :: Position -> Position -> Bool -> ShogiGame Bool
tryHorseMove (srcRow, srcCol) (desRow, desCol) isPromoted = do
    player <- gets currentPlayer

    if isPromoted
        then tryGoldMove (srcRow, srcCol) (desRow, desCol)
    else do
        let deltaX = desRow - srcRow
            deltaY = abs (desCol - srcCol)
            adjustedDeltaX = case player of
                                A -> 2
                                B -> -2
        return $ (deltaX == adjustedDeltaX && deltaY == 1)

tryGoldMove :: Position -> Position -> ShogiGame Bool
tryGoldMove (srcRow, srcCol) (desRow, desCol) = do
    player <- gets currentPlayer

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
    return $ isKingMove && not adjustedDiagonalMove

trySilverMove :: Position -> Position -> Bool -> ShogiGame Bool
trySilverMove (srcRow, srcCol) (desRow, desCol) isPromoted = do
    player <- gets currentPlayer

    if isPromoted
        then tryGoldMove (srcRow, srcCol) (desRow, desCol)
    else do
        let deltaX = desRow - srcRow
            deltaY = desCol - srcCol
            -- Verificar se o movimento é um movimento na diagonal
            isDiagonalMove = (abs deltaX == 1 && abs deltaY == 1)
            -- Ajustar movimento do cavalo baseado no jogador
            isCapture = abs deltaY == 0
            isForwardMove = case player of
                                A -> deltaX == 1
                                B -> deltaX == -1
        return $ isDiagonalMove || (isForwardMove && isCapture)

dropPiece :: Piece -> Position -> ShogiGame ()
dropPiece pieceToReplace toPos = placePiece toPos (Just pieceToReplace)

isValidPosition :: Position -> ShogiGame Bool
isValidPosition (row, col) = return $ row >= 0 && row < 9 && col >= 0 && col < 9