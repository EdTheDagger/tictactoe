import Test.Tasty
import Test.Tasty.Hspec

import TicTacToe

main :: IO ()
main = do
    unitTests <- testSpec "hSpecTests" hTestSpecs
    defaultMain $ testGroup "Tests" [unitTests]

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