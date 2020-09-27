module TicTacToe where

import Data.List.Split ( chunksOf )
import Data.Char (digitToInt, isDigit)
import Control.Monad (forever)

data Cell = E | X | O
    deriving (Show, Eq)
data Board = Board [Cell]
    deriving (Eq)

cellChar :: Cell -> Char
cellChar E = ' '
cellChar X = 'X'
cellChar O = 'O'

instance Show Board where
    show (Board cs) = unlines . reverse . chunksOf 3 . map cellChar $ cs

emptyBoard :: Board
emptyBoard = Board (replicate 9 E)

setCell :: Cell -> Int -> Board -> Board
setCell c i (Board cs) = 
    let (l,(_:rs)) = splitAt i cs
     in Board $ l ++ c:rs

getMove :: IO (Maybe Int)
getMove = do
    line <- getLine
    return $ extractMove line

extractMove :: String -> Maybe Int
extractMove [c] | isDigit c = 
  case digitToInt c of
    0 -> Nothing
    x -> Just x
extractMove _               = Nothing

doMove :: Int -> Board -> Board
doMove = setCell X

runGame :: IO ()
runGame = forever $ do
    print emptyBoard
    putStrLn "Enter your move"
    possibleMove <- getMove
    case possibleMove of
        Nothing -> putStrLn "Illegal move :("
        Just m  -> print $ doMove (m-1) emptyBoard
