module Text.LaTeX.LambdaTeX.Package where

import           Text.LaTeX.Base.Syntax

import qualified Data.Set                           as S

import           Text.LaTeX.LambdaTeX.Package.Types
import           Text.LaTeX.LambdaTeX.Types

-- | Declare a dependency to a package, with given arguments
packageDep :: Monad m => [LaTeX] -- ^ Arguments
                      -> Text -- ^ Name of the LaTeX package
                      -> ΛTeXT m ()
packageDep args name = λtell $ mempty { outputPackageDependencies = S.singleton (PackageDep name args) }

-- | Declare a dependency to a package without any arguments
packageDep_ :: Monad m => Text -> ΛTeXT m ()
packageDep_ name = packageDep [] name

-- | Inject package dependencies into a given LaTeX document.
--   This is done by top-level functions in @Text.LaTeX.LambdaTeX@ automatically
injectPackageDependencies :: [PackageDep] -> LaTeX -> LaTeX
injectPackageDependencies ps = go
    -- We're looking for this: TeXComm "documentclass" ...
  where
    -- We have to go looking through the LaTeX :(
    go t@(TeXComm "documentclass" _) = TeXSeq t packages
    go (TeXSeq t1 t2) = TeXSeq (go t1) (go t2)
    go c = c

    packages :: LaTeX
    packages = mconcat $ map (\(PackageDep name args) -> usepackage args name) ps


-- | Redefinition of @usepackage@ to use Text
--   Don't use this directly, use the packageDep instead!
usepackage :: [LaTeX] -> Text -> LaTeX
usepackage ls pn = TeXComm "usepackage" [MOptArg ls, FixArg $ TeXRaw pn]

