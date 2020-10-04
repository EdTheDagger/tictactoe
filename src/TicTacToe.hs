module TicTacToe where

import Data.Maybe (isJust)
import Data.List.Split (chunksOf)
import Data.List (find, transpose)
import Data.Char (digitToInt, isDigit)
import Control.Monad (forever)

type Pos = Int
type Player = Piece

data Piece = X | O
    deriving (Show, Eq)
data Cell = E | P Piece  -- E, P X, P O
    deriving (Eq)
data Move = Move Player Pos
data Board = Board [Cell]
    deriving (Eq)

data Command = Exit
    deriving (Show, Eq)
data Input = ICmd Command | IPos Pos | IInvalid
    deriving (Show, Eq)

instance Show Cell where
    show E = "  "
    show (P X) = "❌"
    show (P O) = "⭕️"

instance Show Board where
    show (Board cs) = unlines . map concat . reverse . map (map show) . chunksOf 3 $ cs

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

-- [123456789] -> [147258369]
-- [[123],[456],[789]] -> [[147],[258],[369]]


findWinner :: Board -> Maybe Player
findWinner (Board cs) = 
    let chunks = chunksOf 3 cs
        diagonals = [map (cs!!) [0,4,8], map (cs!!) [2,4,6]]
     in extractFirstJust $ map getSame $ chunks ++ transpose chunks ++ diagonals
  where
    getSame :: [Cell] -> Maybe Player
    getSame [P x, P y, P z] | x == y && x == z = Just x
    getSame _                                  = Nothing

    extractFirstJust :: [Maybe a] -> Maybe a
    extractFirstJust [] = Nothing
    extractFirstJust (Just x:_) = Just x
    extractFirstJust (_:rest) = extractFirstJust rest

gameStep :: Player -> Board -> IO ()
gameStep currentPlayer board = do
    print board

    case findWinner board of
        Just winner -> putStrLn $ show winner ++ " won the game! :)"
        Nothing -> do
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
