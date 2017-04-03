{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.LaTeX.LambdaTeX.Selection.Arbitrary where

import Test.QuickCheck

import Data.Text.Arbitrary ()
import Text.LaTeX.LambdaTeX.Selection.Types

instance Arbitrary Selector where
    arbitrary = oneof [pure All, Match <$> arbitrary, Ignore <$> arbitrary]
