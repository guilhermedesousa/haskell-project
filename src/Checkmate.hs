module Checkmate where

import Piece hiding (isPromoted)
import Board
import Utils
import Player
import Moviments

-- Verifica se o rei está em cheque
isKingInCheck :: Player -> Board -> Bool
isKingInCheck player board =
    let kingPos = findKingCoordinate board player
    in any (\pos ->
        case getPieceFromPosition pos board of
            Just (Piece pieceType piecePlayer isPromoted) ->
                piecePlayer == opponent player && isValidMove pos kingPos board
            _ -> False
    ) allPositions

-- Gera todos os movimentos legais para um jogador
-- allLegalMoves :: Player -> Board -> [(Position, Position)]
-- allLegalMoves player board =
--     concatMap (\(pos, piece) -> legalMovesForPiece (pos, piece) board) (playerPieces player board)

-- playerPieces :: Player -> Board -> [(Position, Piece)]
-- playerPieces player board = 
--     [((r, c), piece) | r <- [0..8], c <- [0..8],
--                       let pos = (r, c),
--                       Just piece <- [getPieceFromPosition pos board],
--                       getPlayer piece == player]

-- Função fictícia que verifica se o movimento é permitido para uma peça
-- Esta função deve ser implementada de acordo com as regras do seu jogo
-- isMoveAllowed :: Piece -> Position -> Position -> Board -> Bool
-- isMoveAllowed (Piece pieceType player isPromoted) fromPos toPos board =
--     -- Aqui você deve implementar a lógica de movimento para cada peça
--     -- Por enquanto, vamos apenas retornar True para simplificar
--     True

-- -- Função para verificar se um movimento é seguro
-- isMoveSafe :: (Position, Position) -> Player -> Board -> Bool
-- isMoveSafe (fromPos, toPos) player board =
--     maybe False (not . isKingInCheck player) (movePiece fromPos toPos board)

isCheckmate :: Player -> Board -> Bool
isCheckmate player board =
    isKingInCheck player board && canNotScape player board

canNotScape :: Player -> Board -> Bool
canNotScape player board =
    all (isKingStillInCheck player) possibleBoards
  where
    -- Todas as posições das peças do jogador
    playerPositions = playerPiecePositions player board

    -- Gera todos os tabuleiros possíveis após mover uma peça
    possibleBoards = [newBoard | fromPos <- playerPositions,
                                 toPos <- allPositions,
                                 isValidPosition toPos,
                                 Just newBoard <- [movePiece fromPos toPos board]]

findEscapeMove :: Player -> Board -> Maybe (Position, Position)
findEscapeMove player board = 
    case [ (fromPos, toPos) 
         | (fromPos, toPos, newBoard) <- possibleMoves
         , not (isKingStillInCheck player newBoard)
         ] of
        [] -> Nothing
        (move:_) -> Just move
  where
    -- Todas as posições das peças do jogador
    playerPositions = playerPiecePositions player board
    
    -- Gera todos os movimentos possíveis e seus respectivos tabuleiros
    possibleMoves = [(fromPos, toPos, newBoard) | 
                      fromPos <- playerPositions,
                      toPos <- allPositions,
                      isValidPosition toPos,
                      Just newBoard <- [movePiece fromPos toPos board]]

-- Verifica se o rei ainda está em xeque no novo tabuleiro
isKingStillInCheck :: Player -> Board -> Bool
isKingStillInCheck = isKingInCheck

escapePositions :: Player -> Board -> [(Position, Position)]
escapePositions player board =
    let inCheck = isKingInCheck player board
        -- Obtém todas as posições das peças do jogador
        playerPositions = playerPiecePositions player board
        -- Gera todos os movimentos possíveis para cada peça
        possibleMoves = [(fromPos, toPos) | fromPos <- playerPositions, toPos <- allPositions]
        -- Verifica se algum movimento tira o rei do xeque
        isMoveSafe (fromPos, toPos) =
            case movePiece fromPos toPos board of
                Just newBoard -> not (isKingInCheck player newBoard)
                Nothing -> False
        -- Filtra as posições que escapam do xeque
    in if inCheck
       then filter isMoveSafe possibleMoves
       else [] -- Se o rei não está em xeque, não há posições para escapar


-- Função auxiliar que retorna todas as posições das peças de um jogador no tabuleiro
playerPiecePositions :: Player -> Board -> [Position]
playerPiecePositions player board =
    [(r, c) | r <- [0..8], c <- [0..8],
              let pos = (r, c),
              Just piece <- [getPieceFromPosition pos board],
              getPlayer piece == player]

-- legalMovesForPiece :: (Position, Piece) -> Board -> [(Position, Position)]
-- legalMovesForPiece (fromPos, piece) board =
--     [(fromPos, toPos) | toPos <- allPositions, isValidMove fromPos toPos board]

-- Função para encontrar a posição do rei do jogador
findKingCoordinate :: Board -> Player -> Position
findKingCoordinate board player =
    head [pos | pos <- allPositions, isKing pos]
  where
    isKing pos = case getPieceFromPosition pos board of
        Just (Piece Rei piecePlayer _) -> piecePlayer == player
        _ -> False

-- Função que retorna o jogador oponente
opponent :: Player -> Player
opponent A = B
opponent B = A
