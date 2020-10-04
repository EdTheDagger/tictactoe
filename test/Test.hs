import Test.Tasty
import Test.Tasty.Hspec

import TicTacToe

main :: IO ()
main = do
    unitTests <- testSpec "hSpecTests" hTestSpecs
    defaultMain $ testGroup "Tests" [unitTests]

xInTheCenter :: Board
xInTheCenter = Board [E, E, E, E, P X, E, E, E, E]

putXInTheCenter :: Move
putXInTheCenter = Move X 5

hTestSpecs :: Spec
hTestSpecs = 
  describe "Unit tests" $ do
    it "extractPos reads empty string" $
      extractPos "" `shouldBe` Nothing
    it "extractPos reads 0" $
      extractPos "0" `shouldBe` Nothing
    it "extractPos reads 1" $
      extractPos "1" `shouldBe` Just 1
    it "extractPos reads 5" $
      extractPos "5" `shouldBe` Just 5
    it "extractPos reads a" $
      extractPos "a" `shouldBe` Nothing
    it "extractPos reads x" $
      extractPos "x" `shouldBe` Nothing
    it "extractPos reads 12" $
      extractPos "12" `shouldBe` Nothing

    it "setCell X in the center" $
      setCell (P X) 5 emptyBoard `shouldBe` xInTheCenter

    it "place X in the center" $
      doMove putXInTheCenter emptyBoard `shouldBe` xInTheCenter

    it "getCell of 1 takes fist" $
      getCell 1 (Board [P X, E, E, E, E, E, E, E, E]) `shouldBe` P X
    it "isValidMove is True on empty boards" $
      isValidMove putXInTheCenter emptyBoard `shouldBe` True
    it "isValidMove is True on empty boards" $
      isValidMove putXInTheCenter xInTheCenter `shouldBe` False
    it "isValidMove is True on empty boards" $
      isValidMove (Move O 5) xInTheCenter `shouldBe` False
    
    it "findWinner does not find winners that are losers" $
      findWinner (Board [E, E, P X, P O, P X, P O, P O, E, E]) `shouldBe` Nothing
    it "findWinner in rows" $
      findWinner (Board [E, E, P X, P O, P O, P O, P O, E, E]) `shouldBe` Just O
    it "findWinner in cols" $
      findWinner (Board [P O, E, E, P O, P X, P X, P O, P X, E]) `shouldBe` Just O
    it "findWinner in diagonal 1" $
      findWinner (Board [P O, E, E, E, P O, P X, E, P X, P O]) `shouldBe` Just O
    it "findWinner in diagonal 2" $
      findWinner (Board [P X, E, P O, E, P O, P X, P O, P X, E]) `shouldBe` Just O