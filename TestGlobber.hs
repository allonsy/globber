{- Alec Snyder
- hw 10 Globber Test Suite 
- github repo: https://github.com/allonsy/globber
-}
module Main (main) where

import Test.Hspec

import Globber

main :: IO ()
main = hspec $ describe "Testing Globber" $ do

    describe "empty pattern" $ do
      it "matches empty string" $
        matchGlob "" "" `shouldBe` True
      it "shouldn't match non-empty string" $
        matchGlob "" "string" `shouldBe` False
    
    describe "String literals" $ do
      it "matches literal string" $
        matchGlob "hello" "hello" `shouldBe` True
      it "shouldn't match literal string" $
        matchGlob "hello" "string" `shouldBe` False
    
    describe "Question Marks" $ do
      it "should match question mark" $
        matchGlob "h?llo" "hallo" `shouldBe` True
      it "Shouldn't match the empty string" $
        matchGlob "?" "" `shouldBe` False
      it "Shouldn't match more empty strings" $
        matchGlob "hello?" "hello" `shouldBe` False
      it "shouldn't match question mark" $
        matchGlob "hel?o" "hallo" `shouldBe` False

    describe "Asterisks" $ do
      it "Matches everything" $
        matchGlob "*" "" `shouldBe` True
      it "matches trailing asterisks" $
        matchGlob "he*" "hello" `shouldBe` True
      it "matches leading asterisk" $
        matchGlob "*.txt" "file.txt" `shouldBe` True
      it "matches double asterisk" $
        matchGlob "*l*" "hello" `shouldBe` True
      it "should fail on bad non asterisk match" $
        matchGlob "*.txt" "file.tar" `shouldBe` False
        
    describe "Escape Sequences" $ do --Note that when we encode strings here, the backslash is escaped so that the backslash is part of the string"
        it "matches escaped regular character" $
            matchGlob "\\h\\e\\l\\l\\o" "hello" `shouldBe` True
        it "matches escaped questions" $
            matchGlob "hello\\?" "hello?" `shouldBe` True
        it "matches escaped asterisk" $
            matchGlob "\\*hello\\*" "*hello*" `shouldBe` True
        it "matches escaped backslash" $
            matchGlob "\\\\hello" "\\hello" `shouldBe` True
        it "shouldn't glob on escaped asterisk" $
            matchGlob "\\*.txt" "file.txt" `shouldBe` False
