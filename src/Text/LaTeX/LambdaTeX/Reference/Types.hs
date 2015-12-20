module Text.LaTeX.LambdaTeX.Reference.Types where

import           Data.Ord  (comparing)
import           Data.Text (Text)

type ReferenceType = Text
data Reference = Reference {
      referenceType   :: ReferenceType
    , referenceName   :: Text
    , referenceFields :: [(Text, Text)]
  } deriving (Show, Eq)

instance Ord Reference where
    compare = comparing referenceName


