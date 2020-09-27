module TicTacToe where

import Data.List.Split ( chunksOf )
import Data.Char (digitToInt, isDigit)
import Control.Monad (forever)

data Cell = E | X | O
    deriving (Show, Eq)
data Board = Board [Cell]
    deriving (Eq)

instance Show Board where
    show (Board cs) = unlines . map concat . chunksOf 3 . map show $ cs

emptyBoard :: Board
emptyBoard = Board (replicate 9 E)

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

runGame :: IO ()
runGame = forever $ do
    print emptyBoard
    putStrLn "Enter your move"
    possibleMove <- getMove
    case possibleMove of
        Nothing -> putStrLn "Illegal move :("
        Just m  -> putStrLn $ "Ok, I'll do " ++ show m
