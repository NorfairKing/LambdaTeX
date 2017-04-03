module Text.LaTeX.LambdaTeX.Package.Types where

import Data.Ord (comparing)
import Data.Text (Text)

import Text.LaTeX.Base.Syntax (LaTeX)

data PackageDep = PackageDep
    { packageDepName :: Text
    , packageDepOptions :: [LaTeX]
    } deriving (Show, Eq)

instance Ord PackageDep where
    compare = comparing packageDepName
