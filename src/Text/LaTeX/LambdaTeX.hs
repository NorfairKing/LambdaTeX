-- |
-- TODO(kerckhove) big example here!
--
{-# LANGUAGE OverloadedStrings #-}

module Text.LaTeX.LambdaTeX
    ( module Text.LaTeX.LambdaTeX
    -- ** Selections
    , module Text.LaTeX.LambdaTeX.Selection
    -- ** References
    , module Text.LaTeX.LambdaTeX.Reference
    -- ** Packages dependencies
    , module Text.LaTeX.LambdaTeX.Package
    -- ** IO dependencies
    , module Text.LaTeX.LambdaTeX.Action
    -- ** Re-exports
    , module Text.LaTeX.LambdaTeX.Types
    ) where

import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO(..))

import Control.Concurrent.Async (async, wait)
import System.FilePath

import qualified Data.Set as S
import qualified Data.Text.IO as T

import Text.LaTeX.Base (LaTeX, renderFile)

import Text.LaTeX.LambdaTeX.Action
import Text.LaTeX.LambdaTeX.Package
import Text.LaTeX.LambdaTeX.Package.Internal
import Text.LaTeX.LambdaTeX.Reference
import Text.LaTeX.LambdaTeX.Reference.Internal
import Text.LaTeX.LambdaTeX.Reference.Types
import Text.LaTeX.LambdaTeX.Selection
import Text.LaTeX.LambdaTeX.Selection.Types
import Text.LaTeX.LambdaTeX.Types
import Text.LaTeX.LambdaTeX.Utils

-- | Build all the files for a LaTeX project given by a ΛTeXT generator
--   This either returns Left with an error or Right () to signify success.
--
--   This function takes care of some of the LaTeX tediousness:
--
--      * LaTeX file generation
--      * Automatic bibtex file generation
--      * All safety provided by 'execLambdaTeXT' (in the form of textual errors)
--      * Automatic asynchronic resolution of IO dependencies for graphviz or tikz figures
buildLaTeXProject ::
       MonadIO m => ΛTeXT m a -> ProjectConfig -> m (Either [ΛError] ())
buildLaTeXProject func conf = do
    (errs, latex, refs, actions) <-
        execLambdaTeXT func $ projectGenerationConfig conf
    -- Render tex file
    let renderTex = do
            let mainTexFile = projectTexFileName conf ++ ".tex"
            renderFile (projectBuildDir conf </> mainTexFile) latex
    -- Render bib file
    let renderMain = do
            let mainBibFile = projectBibFileName conf ++ ".bib"
            removeIfExists mainBibFile
            T.appendFile (projectBuildDir conf </> mainBibFile) $
                renderReferences refs
    let performAction (name, action) = do
            void action
            putStrLn $ "Job " ++ name ++ " done."
    -- Perform all the IO actions asynchronously
    as <-
        liftIO $ mapM async $ renderTex : renderMain : map performAction actions
    liftIO $ forM_ as wait
    return $
        if null errs
            then Right ()
            else Left errs

-- | Execute a ΛTeXT generation
--   This either returns a tuple of the errors and a tuple of the resulting LaTeX value and a list of external references that need to be put into a bibtex file.
--
--   This function takes care of a lot of safety issues:
--
--      * Subset selection. This allows you to build large documents in parts.
--      * External dependency selection. No more '??' for external references in the output pdf.
--      * Internal dependency safety. No more '??' for external references in the internal pdf.
--      * Package dependency resolution, TODO(kerckhove) with packages in the right order
--      * Dependency selection of figure dependencies on graphviz or tikz figures
execLambdaTeXT ::
       Monad m
    => ΛTeXT m a
    -> GenerationConfig
    -> m ([ΛError], LaTeX, [Reference], [(String, IO ())])
execLambdaTeXT func conf = do
    ((_, latex), _, output) <-
        runΛTeX func (ΛConfig $ generationSelection conf) initState
    let mresult =
            injectPackageDependencies
                (S.toList $ outputPackageDependencies output)
                latex
    let (extraErrs, result) =
            case mresult of
                Nothing -> ([IncompatibleDependencies], latex)
                Just res -> ([], res)
    let refs = S.toList $ outputExternalReferences output
    let actions = outputActions output
    -- Check reference errors
    let made = outputLabelsMade output
        needed = outputLabelsNeeded output
        diff = S.difference needed made
    let referss = map ReferenceMissing $ S.toList diff
    return (extraErrs ++ referss, result, refs, actions)
  where
    initState :: ΛState
    initState = ΛState {stateCurrentPart = emptyPart}

-- * Configuration
-- | Configuration of a ΛTeX project
data ProjectConfig = ProjectConfig
    { projectGenerationConfig :: GenerationConfig
    , projectBibFileName :: String
    , projectTexFileName :: String
    , projectBuildDir :: FilePath
    } deriving (Show, Eq)

-- | Default project configuration.
--
--  Modify this instead of building your own 'ProjectConfig'
defaultProjectConfig :: ProjectConfig
defaultProjectConfig =
    ProjectConfig
    { projectGenerationConfig = defaultGenerationConfig
    , projectBibFileName = "main"
    , projectTexFileName = "main"
    , projectBuildDir = "."
    }

-- | Configuration of ΛTeX generation
newtype GenerationConfig = GenerationConfig
    { generationSelection :: Selection
    } deriving (Show, Eq)

-- | Default generation config.
--
--  Modify this instead of building your own 'GenerationConfig'
defaultGenerationConfig :: GenerationConfig
defaultGenerationConfig = GenerationConfig {generationSelection = [All]}
