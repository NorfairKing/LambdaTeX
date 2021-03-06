-- | This module allows you to automatically resolve LaTeX package dependencies in your document.
--
-- Note: If you want different arguments to a package, you have to specify this by making a 'manual' 'packageDep' declaration before your document.
module Text.LaTeX.LambdaTeX.Package where

import Text.LaTeX.Base.Syntax

import qualified Data.Set as S

import Text.LaTeX.LambdaTeX.Package.Types
import Text.LaTeX.LambdaTeX.Types

-- | Declare a dependency to a package, with given arguments
packageDep ::
       Monad m
    => [LaTeX] -- ^ Arguments
    -> Text -- ^ Name of the LaTeX package
    -> ΛTeXT m ()
packageDep args name =
    λtell $
    mempty {outputPackageDependencies = S.singleton (PackageDep name args)}

-- | Declare a dependency to a package without any arguments
packageDep_ :: Monad m => Text -> ΛTeXT m ()
packageDep_ = packageDep []
