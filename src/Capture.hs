module Capture (CapturedPieces, addCapturedPiece, removeCapturedPiece, printCapturedPieces, printCapturedPiece, parsePieceChoice, validReplacement) where

import Control.Monad.State
import Data.Maybe (catMaybes, isJust)
import Data.List (intercalate)
import Data.Char (digitToInt)
import Piece
import Player
import Utils
import Board

type CapturedPieces = ([Maybe Piece], [Maybe Piece])

-- addCapturedPiece :: CapturedPieces -> Maybe Piece -> CapturedPieces
-- addCapturedPiece capturedPieces newPiece = case newPiece of
--     Nothing    -> capturedPieces
--     Just piece -> do
--         let player    = getPlayer piece
--         let pieceType = getType piece
--         case player of
--             A -> do
--                 let newPieceB = Just (Piece pieceType B False)
--                 (fst capturedPieces, addToPlayerList (snd capturedPieces) newPieceB)
--             B -> do
--                 let newPieceA = Just (Piece pieceType A False)
--                 (addToPlayerList (fst capturedPieces) newPieceA, snd capturedPieces)

addCapturedPiece :: Maybe Piece -> ShogiGame ()
addCapturedPiece mPiece = do
    capPieces <- gets capturedPieces
    case mPiece of
        Nothing    -> return ()
        Just piece -> do
            let player    = getPlayer piece
            let pieceType = getType piece
            case player of
                A -> do
                    let newPieceB = Just (Piece pieceType B False)
                    let updatedCapPieces = (fst capPieces, addToPlayerList (snd capPieces) newPieceB)
                    modify $ \gs -> gs { capturedPieces = updatedCapPieces }
                B -> do
                    let newPieceA = Just (Piece pieceType A False)
                    let updatedCapPieces = (addToPlayerList (fst capPieces) newPieceA, snd capPieces)
                    modify $ \gs -> gs { capturedPieces = updatedCapPieces }

removeCapturedPiece :: CapturedPieces -> Int -> Piece -> CapturedPieces
removeCapturedPiece capturedPieces i piece = case (getPlayer piece) of
    A -> (removeFromPlayerList (fst capturedPieces) (i - 1), snd capturedPieces)
    B -> (fst capturedPieces, removeFromPlayerList (snd capturedPieces) (i - 1))

addToPlayerList :: [Maybe Piece] -> Maybe Piece -> [Maybe Piece]
addToPlayerList xs capturedPiece = xs ++ [capturedPiece]

removeFromPlayerList :: [Maybe Piece] -> Int -> [Maybe Piece]
removeFromPlayerList xs i = take i xs ++ drop (i + 1) xs

indexCapturedPieces :: [Piece] -> [String]
indexCapturedPieces pieces = zipWith showIndexedPiece pieces [1..]
    where
        showIndexedPiece :: Piece -> Int -> String
        showIndexedPiece piece n = show n ++ ": " ++ pieceToString piece

-- *código para printar as peças capturadas retirado do chatgpt (catMaybes e indexCapturedPieces)
printCapturedPieces :: ShogiGame ()
printCapturedPieces = do
    curPlayer <- gets currentPlayer
    capPieces <- gets capturedPieces

    let pieces = if curPlayer == A then fst capPieces else snd capPieces
    
    lift $ putStrLn $ "Peças capturadas pelo jogador " ++ show curPlayer ++ ": "
    lift $ putStrLn $ intercalate ", " (indexCapturedPieces (catMaybes pieces))
    lift $ putStrLn "\n"

printCapturedPiece :: Maybe Piece -> ShogiGame ()
printCapturedPiece mPiece = do
    case mPiece of
        Nothing    -> lift $ putStrLn "Nenhuma peça capturada.\n"
        Just piece -> lift $ putStrLn $ "Peça capturada: " ++ show (getType piece) ++ "\n"

parsePieceChoice :: Int -> String -> Maybe Int
parsePieceChoice countCaptured playerChoice
    | x < 1 || x > countCaptured = Nothing
    | otherwise                  = Just x
    where
        xChar = playerChoice !! 0
        x = digitToInt xChar

validReplacement :: Maybe Piece -> Position -> Board -> Maybe Bool
validReplacement pieceToReplace (destRow, destCol) board = case (pieceToReplace) of
    Just piece -> do
        case (getType piece) of
            Peao -> do
                let player = getPlayer piece

                -- *código para filtrar apenas valores Just retirado do chatgpt (isJust)
                -- verifica se há outro peão não promovido na mesma coluna
                let colHasUnpromotedPawn = any (\(_, mPiece) -> case mPiece of
                                                                    Just jPiece -> (getType jPiece) == Peao && not (isPromoted jPiece)
                                                                    Nothing     -> False) $ -- verifica se tem algum peão promovido
                                           filter (\(_, mPiece) -> case mPiece of
                                                                        Just jPiece -> (getPlayer jPiece) == player
                                                                        Nothing     -> False) $ -- filtra apenas peças do player
                                           filter (\(_, mPiece) -> isJust mPiece) $ -- filtra apenas células do tipo 'Just Piece'
                                           map (\row -> (row, board !! row !! destCol)) [0..8] -- pega todas as células da coluna destCol

                -- verifica se a posição destino está na última linha
                let isLastRow = if player == A then destRow == 8 else destRow == 0

                if colHasUnpromotedPawn || isLastRow
                    then Just False
                    else Just True
            Lanca -> do
                let player = getPlayer piece

                -- verifica se a posição destino está na última linha
                let isLastRow = if player == A then destRow == 8 else destRow == 0

                if isLastRow
                    then Just False
                    else Just True
            Cavalo -> do
                let player = getPlayer piece

                -- verifica se a posição destinho é na penúltima ou última linha
                let isPenultimate = if player == A then destRow == 7 else destRow == 1
                let isLastRow = if player == A then destRow == 8 else destRow == 0

                if isPenultimate || isLastRow
                    then Just False
                    else Just True
            _ -> Just True
    _ -> Nothing