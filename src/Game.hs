module Game (playShogi, createGame) where

import Data.Maybe (catMaybes)
import Control.Monad.State
import Utils
import Board
import Player
import Piece
import Moviments
import Capture
import Checkmate

createGame :: GameState
createGame = GameState createInitialBoard ([], []) B -- jogador B começa

playShogi :: ShogiGame ()
playShogi = do
    curPlayer <- gets currentPlayer
    b         <- gets board
    capPieces <- gets capturedPieces
    
    lift $ putStrLn $ show curPlayer ++ " está jogando...\n"
    
    printCapturedPieces
    printBoard
    movePiece
    -- isInCheck <- isKingInCheck curPlayer b
    -- lift $ putStrLn $ "O rei de " ++ show curPlayer ++ (if isInCheck then " está em cheque!" else " não está em cheque.")

    -- isInCheckMate <- isCheckmate curPlayer b

    -- if isInCheckMate
    --     then do
    --         let winner = opponent curPlayer
    --         lift $ putStrLn $ "O jogador " ++ show winner ++ " venceu o jogo!"
    --     else do
    --         movePiece

movePiece :: ShogiGame ()
movePiece = do
    lift $ putStrLn "\nInsira uma posição de origem e destino (ex: 3a 4a), 'repor' ou 'sair' para encerrar:"
    input <- lift getLine

    b         <- gets board
    player    <- gets currentPlayer
    capPieces <- gets capturedPieces

    if input == "sair"
        then lift $ putStrLn "Encerrando o programa."
        -- else if input == "repor"
        --     then do
        --         handlePieceReplacement player capturedPieces board
        else do
            let positions = words input
            if length positions /= 2
                then do
                    lift $ putStrLn "Formato inválido. Use o formato '3a 4a'."
                    movePiece 
                else do
                    case positions of
                        [srcPos, destPos] -> do
                            case parsePosition srcPos of
                                Nothing -> do
                                    lift $ putStrLn "Movimento inválido."
                                    movePiece 
                                Just (srcRow, srcCol) -> do
                                    case parsePosition destPos of
                                        Nothing -> do
                                            lift $ putStrLn "Movimento inválido."
                                            movePiece 
                                        Just (destRow, destCol) -> do
                                            pieceAtSrc <- getPieceFromPosition (srcRow, srcCol)
                                            pieceAtDest <- getPieceFromPosition (destRow, destCol)

                                            case pieceAtSrc of
                                                Nothing -> do
                                                    lift $ putStrLn $ "Não há peça na posição " ++ srcPos
                                                    movePiece
                                                Just srcPiece | player /= getPlayer srcPiece -> do
                                                    lift $ putStrLn $ "Movimento inválido: É a vez do jogador " ++ show player
                                                    movePiece 
                                                Just piece -> do
                                                    case pieceAtDest of  
                                                        Just destPiece | getPlayer piece == getPlayer destPiece -> do
                                                            lift $ putStrLn "Movimento inválido: Não pode capturar uma peça do mesmo time."
                                                            movePiece
                                                        _ -> do
                                                            success <- tryMovePiece (srcRow, srcCol) (destRow, destCol)
                                                            if success
                                                                then do
                                                                    lift $ putStrLn $ "\nPeça movida: " ++ show (getType piece) ++ "\n"
                                                                    printCapturedPiece pieceAtDest
                                                                    lift $ putStrLn "--------------------------------------------------"
                                                                    
                                                                    -- atualiza a lista de peças capturadas
                                                                    addCapturedPiece pieceAtDest
                                                                    
                                                                    -- atualiza o jogador da vez
                                                                    modify $ \gs -> gs { currentPlayer = opponent player }

                                                                    playShogi

                                                                    -- isInCheck <- isKingInCheck player
                                                                    -- if isInCheck
                                                                    --     then do
                                                                    --         lift $ putStrLn "Movimento inválido: Não se pode colocar em cheque."
                                                                    --         movePiece
                                                                    --     else do
                                                                    --         -- let updatedCapturedPieces = addCapturedPiece capturedPieces pieceAtDest
                                                                    --         lift $ putStrLn $ "\nPeça na posição " ++ srcPos ++ ": " ++ show (getType piece) ++ "\n"
                                                                    --         modify $ \gs -> gs { currentPlayer = opponent player }
                                                                    --         playShogi
                                                            else do
                                                                lift $ putStrLn "Movimento inválido."
                                                                movePiece
                                                                    
                        _ -> do
                            lift $ putStrLn "Formato inválido. Use o formato '3a 4a'."
                            movePiece

-- handlePieceReplacement :: Player -> CapturedPieces -> Board -> IO ()
-- handlePieceReplacement player capturedPieces board = do
--     let pieces = if player == A then fst capturedPieces else snd capturedPieces
--     let onlyPieces = catMaybes pieces
--     let countCaptured = length onlyPieces
--     if countCaptured == 0
--         then do
--             putStrLn "Não existem peças para repor."
--             movePiece player capturedPieces board  
--         else do
--             putStrLn "\nInforme o número da peça que queira repor + posição de destino (ex: 1 4a), 'mover' ou 'sair' para encerrar:"
--             input <- getLine

--             if input == "mover"
--                 then do
--                     movePiece player capturedPieces board -- Permite mover uma peça
--                 else if input == "sair"
--                     then putStrLn "Encerrando o programa."
--                     else do
--                         let playerChoices = words input
--                         if length playerChoices /= 2
--                             then do
--                                 putStrLn "Formato inválido. Use o formato '1 4a'."
--                                 handlePieceReplacement player capturedPieces board  
--                             else do
--                                 case playerChoices of
--                                     [playerChoice, destPos] -> do
--                                         case parsePieceChoice countCaptured playerChoice of
--                                             Nothing -> do
--                                                 putStrLn "Escolha inválida."
--                                                 handlePieceReplacement player capturedPieces board  
--                                             Just pieceNumber -> do
--                                                 case parsePosition destPos of
--                                                     Nothing -> do
--                                                         putStrLn "Movimento inválido."
--                                                         handlePieceReplacement player capturedPieces board  
--                                                     Just (destRow, destCol) -> do
--                                                         let pieceToReplace = onlyPieces !! (pieceNumber - 1)

--                                                         case (getPieceFromPosition (destRow, destCol) board) of  
--                                                             Just _ -> do
--                                                                 putStrLn "Movimento inválido: Peças são reposicionadas apenas em casas vazias."
--                                                                 handlePieceReplacement player capturedPieces board  
--                                                             _ -> do
--                                                                 case (validReplacement (Just pieceToReplace) (destRow, destCol) board) of
--                                                                     Nothing -> do
--                                                                         putStrLn "Reposição inválida."
--                                                                         handlePieceReplacement player capturedPieces board  
--                                                                     Just False -> do
--                                                                         putStrLn "Reposição inválida."
--                                                                         handlePieceReplacement player capturedPieces board  
--                                                                     _ -> do
--                                                                         let isPieceDropped = dropPiece pieceToReplace (destRow, destCol) board

--                                                                         case isPieceDropped of
--                                                                             Nothing -> do
--                                                                                 putStrLn "Movimento inválido."
--                                                                                 handlePieceReplacement player capturedPieces board  
--                                                                             Just updatedBoard -> do
--                                                                                 -- verifica se é uma reposição de checkmate
--                                                                                 let isInCheckMate = isCheckmate (opponent player) updatedBoard
--                                                                                 if isInCheckMate
--                                                                                     then do
--                                                                                         putStrLn "Reposição inválida: não é permitido reposicionar dando chequemate"
--                                                                                         handlePieceReplacement player capturedPieces board  
--                                                                                     else do
--                                                                                         let updatedCapturedPieces = removeCapturedPiece capturedPieces pieceNumber pieceToReplace
--                                                                                         let nextPlayer = if player == A then B else A
--                                                                                         playShogi nextPlayer updatedBoard updatedCapturedPieces -- Chama a função para jogar novamente
--                                     _ -> do
--                                         putStrLn "Formato inválido. Use o formato '1 4a'."
--                                         handlePieceReplacement player capturedPieces board  