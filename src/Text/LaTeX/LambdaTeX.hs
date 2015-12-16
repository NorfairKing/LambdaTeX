{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Text.LaTeX.LambdaTeX (
      note
    , execLambdaTeXT

    , module Text.LaTeX.LambdaTeX.Types
    , module Text.LaTeX.LambdaTeX.Reference
    , module Text.LaTeX.LambdaTeX.Reference.Types

    ) where

import           Control.Monad                        (when)

import qualified Data.Set                             as S

import           Text.LaTeX.Base                      (LaTeX)

import           Text.LaTeX.LambdaTeX.Package
import           Text.LaTeX.LambdaTeX.Reference
import           Text.LaTeX.LambdaTeX.Reference.Types
import           Text.LaTeX.LambdaTeX.Selection
import           Text.LaTeX.LambdaTeX.Selection.Types
import           Text.LaTeX.LambdaTeX.Types

execLambdaTeXT :: Monad m => ΛTeXT m a -> Selection -> m (Either String (LaTeX, [Reference]))
execLambdaTeXT func conf = do
    ((_,latex), _, output) <- runΛTeX func (Config conf) initState
    let result = injectPackageDependencies (S.toList $ outputPackageDependencies output) latex
    let refs = S.toList $ outputExternalReferences output
    -- TODO(kerckhove) Make bibtex file?
    -- TODO(kerckhove) Check internal references
    return $ Right (result, refs)

  where
    initState :: ΛState
    initState = State { stateCurrentPart = emptyPart }

-- | Make a sub-part of the document with a name.
--   This allows you to use the subset-selection feature later.
note :: Monad m => Text -> ΛTeXT m () -> ΛTeXT m ()
note partname func = do
    pushCurrentPart partname

    s <- isSelected
    when s func

    popCurrentPart


isSelected :: Monad m => ΛTeXT m Bool
isSelected = do
    part <- currentPart
    sels <- λasks configSelection
    return $ selects part sels

currentPart :: Monad m => ΛTeXT m Part
currentPart = λgets stateCurrentPart

pushCurrentPart :: Monad m => Text -> ΛTeXT m ()
pushCurrentPart partname = λmodify (\s -> s { stateCurrentPart = pushPart (stateCurrentPart s) partname})

popCurrentPart :: Monad m => ΛTeXT m ()
popCurrentPart = λmodify (\s -> s { stateCurrentPart = popPart $ stateCurrentPart s })


