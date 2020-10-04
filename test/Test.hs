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
    it "extractMove reads empty string" $
      extractMove "" `shouldBe` Nothing
    it "extractMove reads 0" $
      extractMove "0" `shouldBe` Nothing
    it "extractMove reads 1" $
      extractMove "1" `shouldBe` Just 1
    it "extractMove reads 5" $
      extractMove "5" `shouldBe` Just 5
    it "extractMove reads a" $
      extractMove "a" `shouldBe` Nothing
    it "extractMove reads x" $
      extractMove "x" `shouldBe` Nothing
    it "extractMove reads 12" $
      extractMove "12" `shouldBe` Nothing

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