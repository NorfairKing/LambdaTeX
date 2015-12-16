{-# LANGUAGE OverloadedStrings #-}
module Text.LaTeX.LambdaTeX.Reference where

import           Text.LaTeX.Base.Class
import           Text.LaTeX.Base.Syntax

import qualified Text.LaTeX.Base.Commands                as H (label, pageref,
                                                               ref)

import           Text.LaTeX.LambdaTeX.Reference.Internal
import           Text.LaTeX.LambdaTeX.Reference.Types
import           Text.LaTeX.LambdaTeX.Types

-- * Internal references
-- An internal references point is defined by a label
-- It is then referenced with @ref label@
--
-- ΛTeX automatically ensures that those references are always internally consistent
-- BUT you have to make sure that you use these functions instead of the regular ones.

-- ** Labels

-- | Label a part
label :: Monad m => Text -> ΛTeXT m ()
label l = do
    fromLaTeX $ H.label $ TeXRaw l
    addLabelMade l

-- | The safe version of LaTeX's @ref@
ref :: Monad m => Text -> ΛTeXT m ()
ref l = do
    fromLaTeX $ H.ref $ TeXRaw l
    addLabelNeeded l

-- | The safe version of LaTeX's @pageref@
pageref :: Monad m => Text -> ΛTeXT m ()
pageref l = do
    fromLaTeX $ H.pageref $ TeXRaw l
    addLabelNeeded l

-- * External references
-- ** Defining references

-- TODO(kerckhove) Check for duplicate fields?

-- | Define an external reference.
--   Use 'cite' or 'nocite' to use this in your document.
makeReference :: ReferenceType -> Text -> [(Text, Text)] -> Reference
makeReference = Reference

-- ** Using references

-- | Refer to an external reference.
cite :: Monad m => Reference -> ΛTeXT m ()
cite ref = do
  fromLaTeX $ comm1 "cite" $ TeXRaw $ referenceName ref
  addReference ref

-- | Put an external reference in your list of references without using it.
nocite :: Monad m => Reference -> ΛTeXT m ()
nocite ref = do
  fromLaTeX $ comm1 "nocite" $ TeXRaw $ referenceName ref
  addReference ref

-- ** Reference Types
--
-- See [The Wiki article on which fields each of them requires](https://en.wikibooks.org/wiki/LaTeX/Bibliography_Management)
--
-- TODO(kerckhove) Generate these!
unpublished :: ReferenceType
unpublished = "unpublished"

lectureSlides :: ReferenceType
lectureSlides = "unpublished"

online :: ReferenceType
online = "online"

article :: ReferenceType
article = "article"

