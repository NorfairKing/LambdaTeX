module Text.LaTeX.LambdaTeX.Package.Internal where

import           Text.LaTeX.Base.Syntax

import           Text.LaTeX.LambdaTeX.Package.Types
import           Text.LaTeX.LambdaTeX.Types

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

