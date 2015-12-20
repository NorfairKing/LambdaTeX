module Text.LaTeX.LambdaTeX.Selection.Internal where

import           Data.List                            (isPrefixOf)

import           Text.LaTeX.LambdaTeX.Selection.Types
import           Text.LaTeX.LambdaTeX.Types

-- | Whether a part is selected by a given list of selectors.
--
-- A part is selected if:
--
-- All parts are selected or
-- It it is matched and not ignored.
selects :: Part -> [Selector] -> Bool
selects (Part ps) ss = go ps ss False
    where
        go :: [Text] -> [Selector] -> Bool -> Bool
        go _ [] b                       = b
        go ps (All:ss) _                = go ps ss True
        go ps ((Match s):ss) b          = if ps `matches` s
                                          then go ps ss True
                                          else go ps ss b
        go ps ((Ignore s):ss) True      = if ps `matches` s
                                          then go ps ss False
                                          else go ps ss True
        go ps ((Ignore _):ss) False     = go ps ss False

-- | Whether the current part is selected to be generated
isSelected :: Monad m => ΛTeXT m Bool
isSelected = do
    part <- currentPart
    sels <- λasks configSelection
    return $ selects part sels

-- * Current part Helpers

-- | Returns the current part from the ΛTeX state
currentPart :: Monad m => ΛTeXT m Part
currentPart = λgets stateCurrentPart

-- | Execute the following function with a given sub-part name
inPart :: Monad m => Text -> ΛTeXT m () -> ΛTeXT m ()
inPart partname func = do
    pushCurrentPart partname
    func
    popCurrentPart

-- | Push a new part name onto the current part stack
--
-- WARNING: This should never be used outside of 'inPart'
pushCurrentPart :: Monad m => Text -> ΛTeXT m ()
pushCurrentPart partname = λmodify (\s -> s { stateCurrentPart = pushPart (stateCurrentPart s) partname})

-- | Pop a part name off the current part stack
--
-- WARNING: This should never be used outside of 'inPart'
popCurrentPart :: Monad m => ΛTeXT m ()
popCurrentPart = λmodify (\s -> s { stateCurrentPart = popPart $ stateCurrentPart s })


-- * Text Helpers

-- | Split a string on a dot.
-- This can be useful to convert from a 'Selector's textual representation back to a selector
--
-- > split = splitOn '.'
split :: String -> [String]
split = splitOn '.'

-- | Split a string on a specific character
splitOn :: Char -> String -> [String]
splitOn c s = go s []
  where
    go :: String -> String -> [String]
    go [] s = [s]
    go (sc:ss) acc | sc == c   = acc : go [] ss
                   | otherwise = go ss (acc ++ [sc])


-- | Tests whether a given part matches a given selector's part specification
matches :: [Text] -> [Text] -> Bool
matches = flip isPrefixOf
