{-# LANGUAGE OverloadedStrings #-}
module Text.LaTeX.LambdaTeX.Package.Internal where

import           Control.Monad
import           Data.List

import           Text.LaTeX.Base.Syntax

import           Text.LaTeX.LambdaTeX.Package.Types
import           Text.LaTeX.LambdaTeX.Types

-- | Inject package dependencies into a given LaTeX document.
--   This is done by top-level functions in @Text.LaTeX.LambdaTeX@ automatically
injectPackageDependencies :: [PackageDep] -> LaTeX -> Maybe LaTeX
injectPackageDependencies packages latex = do
    ps <- reorderPackages packages
    let packageDecs = mconcat $ map (\(PackageDep name args) -> usepackage args name) ps
    pure (inject packageDecs latex)
  where
    inject ps = go
      where
        -- We're looking for this: TeXComm "documentclass" ...
        -- We have to go looking through the LaTeX :(
        go t@(TeXComm "documentclass" _) = TeXSeq t ps
        go (TeXSeq t1 t2) = TeXSeq (go t1) (go t2)
        go c = c



reorderPackages :: [PackageDep] -> Maybe [PackageDep]
reorderPackages deps = do
    bestEffort <- foldM (flip reorderToSatisfy) deps allPackageCombinationRules
    if all (`ruleIsSatisfied` bestEffort) allPackageCombinationRules
    then Just bestEffort
    else Nothing

data PackageRule
    = PackageOrder Text Text
    deriving (Show, Eq)

-- Yes it's dumb, but it works for now.
reorderToSatisfy :: PackageRule -> [PackageDep] -> Maybe [PackageDep]
reorderToSatisfy (PackageOrder n1 n2) ps = Just $
    case (,) <$> elemIndex n1 names <*> elemIndex n2 names of
        Nothing -> ps
        Just (ix1, ix2) ->
            if ix1 < ix2
            then ps
            else (ps !! ix1)
                : (ps !! ix2)
                : map snd (filter (\(ix, _) -> ix /= ix1 && ix /= ix2) (zip [0..] ps))
  where names = map packageDepName ps

ruleIsSatisfied :: PackageRule -> [PackageDep] -> Bool
ruleIsSatisfied (PackageOrder n1 n2) ps
    = case (<) <$> elemIndex n1 names <*> elemIndex n2 names of
        Just False -> False
        _ -> True
  where names = map packageDepName ps

allPackageCombinationRules :: [PackageRule]
allPackageCombinationRules =
    [ mintedLibertineRule
    ]

mintedLibertineRule :: PackageRule
mintedLibertineRule = PackageOrder "minted" "libertine"

-- | Redefinition of @usepackage@ to use Text
--   Don't use this directly, use the packageDep instead!
usepackage :: [LaTeX] -> Text -> LaTeX
usepackage ls pn = TeXComm "usepackage" [MOptArg ls, FixArg $ TeXRaw pn]

