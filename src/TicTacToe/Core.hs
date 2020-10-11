module TicTacToe.Core where

import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Char (digitToInt, isDigit)

import TicTacToe.Types

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

isValidMove :: Move -> Board -> Bool
isValidMove (Move _ pos) board = getCell pos board == E


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


performMove :: GameState -> Move -> Maybe GameState
performMove (GameState board currentPlayer) move@(Move movePlayer pos)
  | currentPlayer /= movePlayer  = Nothing
  | pos < 1 || pos > 9           = Nothing
  | not (isValidMove move board) = Nothing
  | otherwise = 
      Just $ GameState (doMove move board) newPlayer
        where
            doMove :: Move -> Board -> Board
            doMove (Move piece pos) = setCell (P piece) pos

            newPlayer = if currentPlayer == X then O else X

