module Text.LaTeX.LambdaTeX.Error where

import Data.Text (Text)

data ΛError
    = ReferenceMissing Text
    | IncompatibleDependencies
    deriving (Show, Eq)
