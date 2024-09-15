module Game (playShogi, createGame) where

import Data.Maybe (catMaybes)
import Control.Monad.State
import Control.Monad (when)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import Control.Monad (liftM2)
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
    
    lift $ putStrLn $ "\n" ++ show curPlayer ++ " está jogando...\n"
    
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
    input <- promptForMove

    if input == "sair"
        then lift $ putStrLn "Encerrando o programa."
    else if input == "repor"
        then lift $ putStrLn "Repondo peca."
    else do
        player <- gets currentPlayer

        -- *exemplo de computação que pode falhar retirado do claude.ai
        mMove <- runMaybeT $ do
            (srcPos, destPos)     <- MaybeT $ return $ parseInput input
            (srcCoord, destCoord) <- MaybeT $ return $ liftM2 (,) (parsePosition srcPos) (parsePosition destPos)
            srcPiece              <- MaybeT $ getPieceFromPosition srcCoord
            result                <- lift $ validateMove player srcCoord destCoord srcPiece
            return (srcCoord, destCoord, srcPiece)

        case mMove of
            Nothing -> do
                lift $ putStrLn "Movimento inválido."
                movePiece
            Just (srcCoord, destCoord, srcPiece) -> do
                destPiece <- getPieceFromPosition destCoord
                success <- tryMovePiece srcCoord destCoord
                if success
                    then handleSuccessMove srcCoord destCoord srcPiece destPiece
                else do
                    lift $ putStrLn "Movimento inválido."
                    movePiece

promptForMove :: ShogiGame String
promptForMove = do
    lift $ putStrLn "\nInsira uma posição de origem e destino (ex: 3a 4a), 'repor' ou 'sair' para encerrar:"
    lift getLine

parseInput :: String -> Maybe (String, String)
parseInput input = case words input of
    [src, dest] -> Just (src, dest)
    _           -> Nothing

validateMove :: Player -> Position -> Position -> Piece -> ShogiGame (Maybe Bool)
validateMove player srcCoord destCoord srcPiece = do
    if player /= getPlayer srcPiece
        then return Nothing
    else do
        destPiece <- getPieceFromPosition destCoord
        case destPiece of
            Just dp | getPlayer srcPiece == getPlayer dp -> return Nothing
            _ -> return $ Just True

handleSuccessMove :: Position -> Position -> Piece -> Maybe Piece -> ShogiGame ()
handleSuccessMove srcCoord destCoord srcPiece destPiece = do
    lift $ putStrLn $ "\nPeça movida: " ++ show (getType srcPiece) ++ "\n"
    
    printCapturedPiece destPiece
    lift $ putStrLn "--------------------------------------------------"
    
    addCapturedPiece destPiece
    
    modify $ \gs -> gs { currentPlayer = opponent (currentPlayer gs) }
    
    playShogi

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