module Text.LaTeX.LambdaTeX.Package where

import           Text.LaTeX.Base.Syntax

import qualified Data.Set                           as S

import           Control.Monad.Writer               (tell)

import           Text.LaTeX.LambdaTeX.Package.Types
import           Text.LaTeX.LambdaTeX.Types

packageDep_ :: Monad m => Text -> ΛTeXT m ()
packageDep_ name = packageDep name []

injectPackageDependencies :: [PackageDep] -> LaTeX -> LaTeX
injectPackageDependencies ps = go
    -- We're looking for this: TeXComm "documentclass" ...
  where
    -- We have to go looking through the LaTeX :(
    go t@(TeXComm "documentclass" _) = TeXSeq t packages
    go (TeXSeq t1 t2) = TeXSeq (go t1) (go t2)
    go c = c

    -- TODO(kerckhove) Make sure that packages end up in the right order
    packages :: LaTeX
    packages = mconcat $ map (\(PackageDep name args) -> usepackage args name) ps


usepackage :: [LaTeX] -> Text -> LaTeX
usepackage ls pn = TeXComm "usepackage" [MOptArg ls, FixArg $ TeXRaw pn]

packageDep :: Monad m => Text -> [LaTeX] -> ΛTeXT m ()
packageDep name args = λtell $ mempty { outputPackageDependencies = S.singleton (PackageDep name args) }
