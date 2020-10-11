module TicTacToe.Core where

import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Char (digitToInt, isDigit)

import TicTacToe.Types
import TicTacToe.AI (AI, trivialAi)

emptyBoard :: Board
emptyBoard = Board (replicate 9 E)

setCell :: Cell -> Pos -> Board -> Board
setCell c i (Board cs) = 
    let (l,(_:rs)) = splitAt (i-1) cs
     in Board $ l ++ c:rs

getCell :: Pos -> Board -> Cell
getCell i (Board cs) = cs !! (i-1)

getInput :: IO Input
getInput = do
    char <- getChar
    return $ case extractPos char of
        Just validPos -> IPos validPos
        Nothing       -> 
            if char == 'q' then
                ICmd Exit
            else
                IInvalid

extractPos :: Char -> Maybe Int
extractPos c | isDigit c = 
  case digitToInt c of
    0 -> Nothing
    x -> Just x
extractPos _               = Nothing

doMove :: Move -> Board -> Board
doMove (Move piece pos) = setCell (P piece) pos

isValidMove :: Move -> Board -> Bool
isValidMove (Move _ pos) board = getCell pos board == E

runGame :: IO ()
runGame = gameStep (GameState emptyBoard X)

gameResult :: Board -> GameResult
gameResult (Board cs) = 
    let chunks = chunksOf 3 cs
        hasEmptyCell = any (==E) cs
        diagonals = [map (cs!!) [0,4,8], map (cs!!) [2,4,6]]
        winner = extractWinner $ map getSame $ chunks ++ transpose chunks ++ diagonals
     in case winner of
          Just p  -> PlayerWon p
          Nothing -> if hasEmptyCell then InProgress else EndedWithoutWinner
  where
    getSame :: [Cell] -> Maybe Player
    getSame [P x, P y, P z] | x == y && x == z = Just x
    getSame _                                  = Nothing

    extractWinner :: [Maybe a] -> Maybe a
    extractWinner [] = Nothing
    extractWinner (Just x:_) = Just x
    extractWinner (_:rest) = extractWinner rest

gameStep :: GameState -> IO ()
gameStep (GameState board currentPlayer) = do
    print board

    case gameResult board of
        PlayerWon winner -> putStrLn $ show winner ++ " won the game! :)"
        EndedWithoutWinner -> putStrLn $ "Game has finished without winner :/"
        InProgress -> do
            putStrLn "Enter your move"
            input <- getInput

            case input of
                IInvalid -> do
                    putStrLn "Illegal move :("
                    gameStep (GameState board currentPlayer)
                ICmd Exit -> putStrLn "Goodbye :)"
                IPos pos -> performPossibleMove (Move currentPlayer pos) board 
  where
    performPossibleMove :: Move -> Board -> IO ()
    performPossibleMove move board | isValidMove move board = do
        let newBoard = doMove move board
        let newPlayer = if currentPlayer == X then O else X
        gameStep (GameState newBoard newPlayer)
    performPossibleMove _ board = do
        putStrLn "Invalid move :("
        gameStep (GameState board currentPlayer)
