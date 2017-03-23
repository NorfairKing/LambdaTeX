{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.LaTeX.LambdaTeX.Part.Arbitrary where

import Test.QuickCheck

import Data.Text.Arbitrary ()
import Text.LaTeX.LambdaTeX.Part

instance Arbitrary Part where
    arbitrary = Part <$> arbitrary
