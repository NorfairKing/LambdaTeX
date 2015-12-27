module Text.LaTeX.LambdaTeX.Selection.Arbitrary where

import           Test.Hspec                           hiding (Selector)
import           Test.QuickCheck

import qualified Data.Text                            as T
import           Data.Text.Arbitrary
import           Text.LaTeX.LambdaTeX.Selection.Types

instance Arbitrary Selector where
    arbitrary = oneof
                    [
                      pure All
                    , Match <$> arbitrary
                    , Ignore <$> arbitrary
                    ]



