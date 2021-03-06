{-# LANGUAGE OverloadedStrings #-}

module Text.LaTeX.LambdaTeX.Reference.Internal where

import qualified Data.Set as S
import qualified Data.Text as T

import Text.LaTeX.LambdaTeX.Reference.Types
import Text.LaTeX.LambdaTeX.Types

-- * Internal references
-- | Declare that a label has been made
addLabelMade :: Monad m => Text -> ΛTeXT m ()
addLabelMade label = λtell $ mempty {outputLabelsMade = S.singleton label}

-- | Declare that a label is needed
addLabelNeeded :: Monad m => Text -> ΛTeXT m ()
addLabelNeeded label = λtell $ mempty {outputLabelsNeeded = S.singleton label}

-- * External references
-- | Render references to bib(La)TeX format
renderReferences :: [Reference] -> Text
renderReferences rs =
    (`mappend` "\n\n") . T.intercalate ",\n\n" $ map showRef rs
  where
    showRef :: Reference -> Text
    showRef r =
        mconcat
            [ "@"
            , referenceType r
            , "{"
            , referenceName r
            , ",\n"
            , T.intercalate ",\n" (map showField $ referenceFields r)
            , "\n}"
            ]
    showField :: (Text, Text) -> Text
    showField (a, b) = mconcat ["  ", a, " = {", b, "}"]

-- | Add a reference to a ΛTeX generator
addReference :: Monad m => Reference -> ΛTeXT m ()
addReference ref = λtell $ mempty {outputExternalReferences = S.singleton ref}
