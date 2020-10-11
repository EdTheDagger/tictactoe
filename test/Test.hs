import Test.Tasty
import Test.Tasty.Hspec

import TicTacToe.Types
import TicTacToe.Core
import TicTacToe.AI

main :: IO ()
main = do
    unitTests <- testSpec "hCoreTestSpecs" hCoreTestSpecs
    aiTests <- testSpec "hAiTestSpecs" hAiTestSpecs
    defaultMain $ testGroup "Tests" [unitTests, aiTests]

xInTheCenter :: Board
xInTheCenter = Board [E, E, E, E, P X, E, E, E, E]

putXInTheCenter :: Move
putXInTheCenter = Move X 5

hCoreTestSpecs :: Spec
hCoreTestSpecs =
  describe "Unit tests" $ do
    describe "extractPos" $ do
      it "reads 0" $
        extractPos '0' `shouldBe` Nothing
      it "reads 1" $
        extractPos '1' `shouldBe` Just 1
      it "reads 5" $
        extractPos '5' `shouldBe` Just 5
      it "reads a" $
        extractPos 'a' `shouldBe` Nothing
      it "reads x" $
        extractPos 'x' `shouldBe` Nothing
      it "reads x" $
        extractPos '\n' `shouldBe` Nothing

    it "setCell X in the center" $
      setCell (P X) 5 emptyBoard `shouldBe` xInTheCenter

    it "place X in the center" $
      performMove (GameState emptyBoard X) putXInTheCenter `shouldBe` Just (GameState xInTheCenter O)

    it "getCell of 1 takes fist" $
      getCell 1 (Board [P X, E, E, E, E, E, E, E, E]) `shouldBe` P X
    
    describe "isValidMove" $ do
      it "is True on empty boards" $
        isValidMove putXInTheCenter emptyBoard `shouldBe` True
      it "is True on empty boards" $
        isValidMove putXInTheCenter xInTheCenter `shouldBe` False
      it "is True on empty boards" $
        isValidMove (Move O 5) xInTheCenter `shouldBe` False
    
    describe "performMove" $ do
      it "doesn't allow overwriting" $
        performMove (GameState xInTheCenter O) (Move O 5) `shouldBe` Nothing
    
    describe "gameResult" $ do
      it "does not find winners that are losers" $
        gameResult (Board [E, E, P X, P O, P X, P O, P O, E, E]) `shouldBe` InProgress
      it "in rows" $
        gameResult (Board [E, E, P X, P O, P O, P O, P O, E, E]) `shouldBe` PlayerWon O
      it "in cols" $
        gameResult (Board [P O, E, E, P O, P X, P X, P O, P X, E]) `shouldBe` PlayerWon O
      it "in diagonal 1" $
        gameResult (Board [P O, E, E, E, P O, P X, E, P X, P O]) `shouldBe` PlayerWon O
      it "in diagonal 2" $
        gameResult (Board [P X, E, P O, E, P O, P X, P O, P X, E]) `shouldBe` PlayerWon O
      it "find end" $
        gameResult (Board [P X, P O, P X, P X, P O, P X, P X, P X, P O]) `shouldBe` PlayerWon X
      it "find end" $
        gameResult (Board [P X, P O, P X, P O, P O, P X, P X, P X, P O]) `shouldBe` EndedWithoutWinner

hAiTestSpecs :: Spec
hAiTestSpecs =
  describe "Unit tests" $ do
    describe "PossibleMoves" $ do
      it "For empty board, all moves are possible" $ do
        possibleMoves (GameState emptyBoard X) `shouldBe` [Move X pos | pos <- [1..9]]


