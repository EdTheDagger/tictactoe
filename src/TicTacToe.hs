module TicTacToe where

import Data.Char (digitToInt, isDigit)
import Control.Monad (forever)

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
    putStrLn "Enter your move"
    possibleMove <- getMove
    case possibleMove of
        Nothing -> putStrLn "Illegal move :("
        Just m  -> putStrLn $ "Ok, I'll do " ++ show m
