module Text.LaTeX.LambdaTeX.Selection.Types where

import           Data.Text (Text)

type Selection = [Selector]
data Selector = All
              | Match [Text]
              | Ignore [Text]
    deriving (Show, Eq)

