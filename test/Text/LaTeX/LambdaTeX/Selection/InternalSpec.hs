{-# LANGUAGE OverloadedStrings #-}
module Text.LaTeX.LambdaTeX.Selection.InternalSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Text.LaTeX.LambdaTeX.Part
import           Text.LaTeX.LambdaTeX.Part.Arbitrary
import           Text.LaTeX.LambdaTeX.Selection
import           Text.LaTeX.LambdaTeX.Selection.Arbitrary
import           Text.LaTeX.LambdaTeX.Selection.Internal
import           Text.LaTeX.LambdaTeX.Selection.Types

spec :: Spec
spec = describe "selects" $ do
    it "is false for the empty selection" $ do
        property $ \p -> selects p [] `shouldBe` False
    it "is true for the 'match all' selection" $ do
        property $ \p -> selects p [All] `shouldBe` True
    it "is true for the empty part and this nonempty selection" $ do
        selects emptyPart [Match ["part"]] `shouldBe` True
    it "matches explicitly matched parts" $ do
        property $ \p -> selects (Part p) [Match p] `shouldBe` True
    it "ignore expicitly ignored parts" $ do
        property $ \p -> selects (Part p) [All, Ignore p] `shouldBe` False
    it "matches the right part for these simple test cases" $ do
        let p = Part ["this", "part"]
        selects p [Match ["this"]] `shouldBe` True
        selects p [Match ["this", "part"]] `shouldBe` True
        selects p [Match ["this", "part", "should"]] `shouldBe` True
        selects p [Match ["this", "part", "should", "be"]] `shouldBe` True
        selects p [Match ["this", "part", "should", "be", "matched"]] `shouldBe` True
        selects p [Match ["something"]] `shouldBe` False
        selects p [Match ["something", "else"]] `shouldBe` False
    it "ignores the right part for these simple test cases" $ do
        let p = Part ["this", "part"]
        selects p [All, Ignore ["this"]] `shouldBe` False
        selects p [All, Ignore ["this", "part"]] `shouldBe` False
        selects p [All, Ignore ["this", "part", "but", "it", "should"]] `shouldBe` True
        selects p [All, Ignore ["this", "part", "but", "it", "should", "be"]] `shouldBe` True
        selects p [All, Ignore ["this", "part", "but", "it", "should", "be", "matched"]] `shouldBe` True
    it "constructs this selection that's more than three subparts long correctly" $ do
        constructSelector "something.in.four.subparts" `shouldBe` Match ["something", "in", "four", "subparts"]




