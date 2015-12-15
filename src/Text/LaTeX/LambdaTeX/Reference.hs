{-# LANGUAGE OverloadedStrings #-}
module Text.LaTeX.LambdaTeX.Reference where

import           Text.LaTeX.Base.Class
import           Text.LaTeX.Base.Syntax

import           Data.List                            (intercalate)
import qualified Data.Set                             as S
import           Data.Text                            (Text)
import qualified Data.Text                            as T

import           Control.Monad.Writer                 (tell)

import           Text.LaTeX.LambdaTeX.Reference.Types
import           Text.LaTeX.LambdaTeX.Types

unpublished :: ReferenceType
unpublished = "unpublished"

lectureSlides :: ReferenceType
lectureSlides = "unpublished"

online :: ReferenceType
online = "online"

article :: ReferenceType
article = "article"

-- * Defining references

-- TODO(kerckhove) Check for duplicate fields?
makeReference :: ReferenceType -> Text -> [(Text, Text)] -> Reference
makeReference = Reference

-- * Using references

cite :: Monad m => Reference -> ΛTeXT m ()
cite ref = do
  fromLaTeX $ comm1 "cite" $ TeXRaw $ referenceName ref
  addReference ref

nocite :: Monad m => Reference -> ΛTeXT m ()
nocite ref = do
  fromLaTeX $ comm1 "nocite" $ TeXRaw $ referenceName ref
  addReference ref

-- * Handling references
showReferences :: [Reference] -> Text
showReferences rs = (`mappend` "\n\n") . T.intercalate ",\n\n" $ map showRef rs
  where
    showRef :: Reference -> Text
    showRef r = mconcat
        [
          "@"
        , referenceType r
        , "{"
        , referenceName r
        , ",\n"
        , T.intercalate ",\n" (map showField $ referenceFields r)
        , "\n}"
        ]

    showField :: (Text, Text) -> Text
    showField (a, b) = mconcat ["  ", a, " = {", b, "}"]

addReference :: Monad m => Reference -> ΛTeXT m ()
addReference ref = λtell $ mempty { outputExternalReferences = S.singleton ref }
