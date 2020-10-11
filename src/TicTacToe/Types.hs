module TicTacToe.Types where

import Data.List.Split (chunksOf)

type Pos = Int
type Player = Piece

data Piece = X | O
    deriving (Show, Eq)
data Cell = E | P Piece  -- E, P X, P O
    deriving (Eq)
data Move = Move Player Pos
    deriving (Eq, Show)
data Board = Board [Cell]
    deriving (Eq)
data GameResult 
        = PlayerWon Player
        | InProgress
        | EndedWithoutWinner
    deriving (Show, Eq)
data GameState = GameState { getBoard :: Board, getCurrentPlayer :: Player }

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
