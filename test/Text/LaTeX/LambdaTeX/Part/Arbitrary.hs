module Text.LaTeX.LambdaTeX.Part.Arbitrary where

import           Test.Hspec                hiding (Selector)
import           Test.QuickCheck

import qualified Data.Text                 as T
import           Data.Text.Arbitrary
import           Text.LaTeX.LambdaTeX.Part

instance Arbitrary Part where
    arbitrary = Part <$> arbitrary



