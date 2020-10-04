module TicTacToe where

import Data.List.Split ( chunksOf )
import Data.Char (digitToInt, isDigit)
import Control.Monad (forever)

type Pos = Int

data Piece = X | O
    deriving (Show, Eq)
data Cell = E | P Piece  -- E, P X, P O
    deriving (Show, Eq)
data Move = Move Piece Pos
data Board = Board [Cell]
    deriving (Eq)

data Command = Exit
    deriving (Show, Eq)
data Input = ICmd Command | IPos Pos | IInvalid
    deriving (Show, Eq)

cellChar :: Cell -> Char
cellChar E = ' '
cellChar (P X) = 'X'
cellChar (P O) = 'O'

instance Show Board where
    show (Board cs) = unlines . reverse . chunksOf 3 . map cellChar $ cs

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
    line <- getLine
    return $ case extractPos line of
        Just validPos -> IPos validPos
        Nothing       -> 
            if line == "q" then
                ICmd Exit
            else
                IInvalid

extractPos :: String -> Maybe Int
extractPos [c] | isDigit c = 
  case digitToInt c of
    0 -> Nothing
    x -> Just x
extractPos _               = Nothing

doMove :: Move -> Board -> Board
doMove (Move piece pos) = setCell (P piece) pos

isValidMove :: Move -> Board -> Bool
isValidMove (Move piece pos) board = getCell pos board == E

runGame :: IO ()
runGame = gameStep X emptyBoard

gameStep :: Piece -> Board -> IO ()
gameStep currentPlayer board = do
    print board
    putStrLn "Enter your move"
    input <- getInput

    case input of
        IInvalid -> do
            putStrLn "Illegal move :("
            gameStep currentPlayer board
        ICmd Exit -> putStrLn "Goodbye :)"
        IPos pos -> performPossibleMove (Move currentPlayer pos) board 
  where
    performPossibleMove :: Move -> Board -> IO ()
    performPossibleMove move board | isValidMove move board = do
        let newBoard = doMove move board
        let newPlayer = if currentPlayer == X then O else X
        gameStep newPlayer newBoard
    performPossibleMove move board = do
        putStrLn "Invalid move :("
        gameStep currentPlayer board
