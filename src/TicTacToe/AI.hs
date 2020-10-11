{-# LANGUAGE TupleSections #-}
module TicTacToe.AI where

import Data.Maybe (mapMaybe)

import TicTacToe.Core
import TicTacToe.Types

data AI = AI { aiMove :: GameState -> Move }

possibleMoves :: GameState -> [Move]
possibleMoves (GameState (Board cs) currentPlayer) = 
    [Move currentPlayer pos | (c, pos) <- zip cs [1..], c == E]

trivialAi :: AI
trivialAi = AI (head . possibleMoves)

oneMoveAi :: AI
oneMoveAi = AI ai
  where
    ai gs = head (winningMoves gs ++ possibleMoves gs)
    winningMoves gs = map fst $ winningStates gs

nextPossibleStates :: GameState -> [(Move, GameState)]
nextPossibleStates gs = 
  mapMaybe (\m -> (m,) <$> performMove gs m) $ possibleMoves gs

winningStates :: GameState -> [(Move, GameState)]
winningStates gs = 
  let p = getCurrentPlayer gs
   in filter (\(_,ngs) -> gameResult (getBoard ngs) == PlayerWon p) (nextPossibleStates gs)

-- TODO: Add AI that avoids moves that let the other player win

