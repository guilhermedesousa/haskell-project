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
    shogiBoard         <- gets board
    -- capPieces <- gets capturedPieces
    
    lift $ putStrLn $ "\n" ++ show curPlayer ++ " está jogando...\n"
    
    printCapturedPieces
    printBoard

    -- kingPos <- findKingCoordinate shogiBoard curPlayer
    -- lift $ putStrLn $ "O rei de " ++ show curPlayer ++ " está na posição: " ++ show kingPos

    -- playerPositions <- playerPiecePositions curPlayer shogiBoard
    -- lift $ putStrLn $ "Posições das peças de " ++ show curPlayer ++ ": " ++ show playerPositions
    
    isInCheck <- isKingInCheck curPlayer shogiBoard
    
    lift $ putStrLn $ "O rei de " ++ show curPlayer ++ (if isInCheck then " está em cheque!" else " não está em cheque.")

    -- maybeEscapeMove <- findEscapeMove curPlayer shogiBoard
    -- case maybeEscapeMove of
    --     Just (fromPos, toPos) -> lift $ putStrLn $ "Movimento de escape encontrado: " ++ show fromPos ++ " para " ++ show toPos
    --     Nothing -> lift $ putStrLn "Nenhum movimento de escape encontrado."

    isInCheckMate <- isCheckmate curPlayer shogiBoard

    if isInCheckMate
        then do
            let winner = opponent curPlayer
            lift $ putStrLn $ "O jogador " ++ show winner ++ " venceu o jogo!"
        else do
            movePiece

movePiece :: ShogiGame ()
movePiece = do
    input <- promptForMove
    case input of
        "sair"  -> lift $ putStrLn "Encerrando o programa."
        "repor" -> replacePiece
        _       -> handleMoveInput input

handleMoveInput :: String -> ShogiGame ()
handleMoveInput input = do
    case parseInput input of
        Nothing -> do
            lift $ putStrLn "Formato inválido. Use o formato '3a 4a'."
            movePiece
        Just (srcPos, destPos) -> do
            let mSrcCoord = parsePosition srcPos
            let mDestCoord = parsePosition destPos
            case (mSrcCoord, mDestCoord) of
                (Nothing, _) -> invalidMovement "Movimento inválido."
                (_, Nothing) -> invalidMovement "Movimento inválido."
                (Just srcCoord, Just destCoord) -> processMovement srcCoord destCoord

invalidMovement :: String -> ShogiGame ()
invalidMovement message = do
    lift $ putStrLn message
    movePiece

processMovement :: Position -> Position -> ShogiGame ()
processMovement srcCoord destCoord = do
    player <- gets currentPlayer
    mSrcPiece  <- getPieceFromPosition srcCoord
    mDestPiece <- getPieceFromPosition destCoord

    case mSrcPiece of
        Nothing -> invalidMovement $ "Não há peça na posição " ++ show srcCoord
        Just srcPiece | player /= getPlayer srcPiece -> invalidMovement $ "Movimento inválido: É a vez do jogador " ++ show player
        Just piece -> do
            case mDestPiece of
                Just destPiece | getPlayer piece == getPlayer destPiece -> invalidMovement $ "Movimento inválido: Não pode capturar uma peça do mesmo time."
                _ -> finalizeMovement srcCoord destCoord piece mDestPiece

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

finalizeMovement :: Position -> Position -> Piece -> Maybe Piece -> ShogiGame ()
finalizeMovement srcCoord destCoord srcPiece destPiece = do
    success <- tryMovePiece srcCoord destCoord
    if success
        then do
            lift $ putStrLn $ "\nPeça movida: " ++ show (getType srcPiece) ++ "\n"
            printCapturedPiece destPiece
            lift $ putStrLn "--------------------------------------------------"
            addCapturedPiece destPiece -- adiciona a peça capturada na lista
            modify $ \gs -> gs { currentPlayer = opponent (currentPlayer gs) } -- muda o jogador da vez
            playShogi
    else invalidMovement "Movimento inválido."

replacePiece :: ShogiGame ()
replacePiece = do
    player <- gets currentPlayer
    pieces <- getCapturedPieces player
    let onlyPieces = catMaybes pieces
    let countCaptured = length onlyPieces
    if countCaptured == 0
        then noCapturedPieces
    else promptForReplacement onlyPieces countCaptured

noCapturedPieces :: ShogiGame ()
noCapturedPieces = do
    lift $ putStrLn "Não existem peças para repor."
    movePiece

promptForReplacement :: [Piece] -> Int -> ShogiGame ()
promptForReplacement onlyPieces countCaptured = do
    input <- promptForReplace
    case input of
        "sair"  -> lift $ putStrLn "Encerrando o programa."
        "mover" -> movePiece
        _       -> handleReplaceInput input onlyPieces countCaptured

handleReplaceInput :: String -> [Piece] -> Int -> ShogiGame ()
handleReplaceInput input onlyPieces countCaptured =
    case parseInput input of
        Nothing -> do
            lift $ putStrLn "Formato inválido. Use o formato '1 4a'."
            replacePiece
        Just (pieceNo, destPos) -> do
            let mPieceNumber = parsePieceChoice countCaptured pieceNo
                mDestCoord = parsePosition destPos
            case (mPieceNumber, mDestCoord) of
                (Nothing, _) -> invalidChoice "Escolha de peça inválida."
                (_, Nothing) -> invalidChoice "Posição de destino inválida."
                (Just pieceNumber, Just destCoord) -> 
                    processReplacement pieceNumber destCoord onlyPieces

invalidChoice :: String -> ShogiGame ()
invalidChoice message = do
    lift $ putStrLn message
    replacePiece

processReplacement :: Int -> Position -> [Piece] -> ShogiGame ()
processReplacement pieceNumber destCoord onlyPieces = do
    let pieceToReplace = onlyPieces !! (pieceNumber - 1)
    dstPiece <- getPieceFromPosition destCoord
    case dstPiece of
        Just _ -> invalidChoice "Movimento inválido: Peças são reposicionadas apenas em casas vazias."
        Nothing -> do
            isValidReplacement <- validReplacement (Just pieceToReplace) destCoord
            if not isValidReplacement
                then invalidChoice "Reposição inválida."
                else finalizeReplacement pieceNumber pieceToReplace destCoord

finalizeReplacement :: Int -> Piece -> Position -> ShogiGame ()
finalizeReplacement pieceNumber pieceToReplace destCoord = do
    dropPiece pieceToReplace destCoord
    lift $ putStrLn "--------------------------------------------------"
    removeCapturedPiece pieceNumber pieceToReplace
    modify $ \gs -> gs { currentPlayer = opponent (currentPlayer gs) }
    playShogi