module Text.LaTeX.LambdaTeX (
    -- * Building ΛTeX
      buildLaTeXProject
    , execLambdaTeXT

    -- * Using ΛTeX
    , note

    -- ** Packages dependencies
    , packageDep
    , packageDep_

    -- ** Re-exports
    , module Text.LaTeX.LambdaTeX.Types
    , module Text.LaTeX.LambdaTeX.Reference
    , module Text.LaTeX.LambdaTeX.Reference.Types

    ) where

import           Control.Monad                        (when)
import           Control.Monad.IO.Class               (MonadIO (..))

import qualified Data.Set                             as S

import           Text.LaTeX.Base                      (LaTeX)

import           Text.LaTeX.LambdaTeX.Package
import           Text.LaTeX.LambdaTeX.Reference
import           Text.LaTeX.LambdaTeX.Reference.Types
import           Text.LaTeX.LambdaTeX.Selection
import           Text.LaTeX.LambdaTeX.Selection.Types
import           Text.LaTeX.LambdaTeX.Types

-- | Build all the files for a LaTeX project given by a ΛTeXT generator
--   This either returns Left with an error or Right () to signify success.
--
--   This function takes care of some of the LaTeX tediousness:
--
--      * LaTeX file generation
--      * Automatic bibtex file generation
--      * All safety provided by 'execLambdaTeXT'
buildLaTeXProject :: MonadIO m => ΛTeXT m a -> Selection -> m (Either String ())
buildLaTeXProject func selec = do
    res <- execLambdaTeXT func selec
    case res of
        Left err -> return $ Left err
        Right _ -> return $ Right ()
    -- TODO(kerckhove) Make bibtex file?

-- | Execute a ΛTeXT generation
--   This either returns Left with an error or Right with the resulting LaTeX value and a list of external references that need to be put into a bibtex file.
--
--   This function takes care of a lot of safety issues:
--
--      * Automatic subset selection. This allows you to build large documents in parts.
--        TODO(kerckhove) allow for faulty documents to build parts!
--          Maybe give specialized Config instead of just selection
--      * Automatic external dependency selection. No more '??' for external references in the output pdf.
--      * TODO(kerckhove) Automatic internal dependency safety. No more '??' for external references in the internal pdf.
--      * Automatic package dependency resolution, TODO(kerckhove) with packages in the right order
execLambdaTeXT :: Monad m => ΛTeXT m a -> Selection -> m (Either String (LaTeX, [Reference]))
execLambdaTeXT func conf = do
    ((_,latex), _, output) <- runΛTeX func (Config conf) initState
    let result = injectPackageDependencies (S.toList $ outputPackageDependencies output) latex
    let refs = S.toList $ outputExternalReferences output
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


