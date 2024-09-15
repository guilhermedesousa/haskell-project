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
        then replacePiece
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

promptForReplace :: ShogiGame String
promptForReplace = do
    lift $ putStrLn "\nInforme o número da peça que queira repor + posição de destino (ex: 1 4a), 'mover' ou 'sair' para encerrar:"
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
    
    -- adiciona a peça capturada na lista
    addCapturedPiece destPiece
    
    -- muda o jogador da vez
    modify $ \gs -> gs { currentPlayer = opponent (currentPlayer gs) }
    
    playShogi

replacePiece :: ShogiGame ()
replacePiece = do
    player <- gets currentPlayer
    pieces <- getCapturedPieces player

    let onlyPieces = catMaybes pieces
    let countCaptured = length onlyPieces

    if countCaptured == 0
        then do
            lift $ putStrLn "Não existem peças para repor."
            movePiece
    else do
        input <- promptForReplace

        if input == "sair"
            then lift $ putStrLn "Encerrando o programa."
        else if input == "mover"
            then movePiece
        else do
            case parseInput input of
                Nothing -> do
                    lift $ putStrLn "Formato inválido. Use o formato '1 4a'."
                    replacePiece
                Just (pieceNo, destPos) -> do
                    let mPieceNumber = parsePieceChoice countCaptured pieceNo
                    let mDestCoord = parsePosition destPos

                    case (mPieceNumber, mDestCoord) of
                        (Nothing, _) -> do
                            lift $ putStrLn "Escolha de peça inválida."
                            replacePiece
                        (_, Nothing) -> do
                            lift $ putStrLn "Posição de destino inválida."
                            replacePiece
                        (Just pieceNumber, Just destCoord) -> do
                            let pieceToReplace = onlyPieces !! (pieceNumber - 1)
                            dstPiece <- getPieceFromPosition destCoord
                            case dstPiece of
                                Just _ -> do
                                    lift $ putStrLn "Movimento inválido: Peças são reposicionadas apenas em casas vazias."
                                    replacePiece
                                _ -> do
                                    isValidReplacement <- validReplacement (Just pieceToReplace) destCoord
                                    case isValidReplacement of
                                        False -> do
                                            lift $ putStrLn "Reposição inválida."
                                            replacePiece
                                        _ -> do
                                            dropPiece pieceToReplace destCoord
                                            
                                            lift $ putStrLn "--------------------------------------------------"
                                            
                                            -- remove a peça capturada da lista
                                            removeCapturedPiece pieceNumber pieceToReplace

                                            -- muda o jogador da vez
                                            modify $ \gs -> gs { currentPlayer = opponent (currentPlayer gs) }

                                            playShogi