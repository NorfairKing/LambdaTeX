module Text.LaTeX.LambdaTeX.Selection where

import           Data.List                            (isPrefixOf)

import qualified Data.Text                            as T

import           Text.LaTeX.LambdaTeX.Selection.Types
import           Text.LaTeX.LambdaTeX.Types

constructSelection :: String -> Selection
constructSelection = map constructSelector . words

constructSelector :: String -> Selector
constructSelector "all" = All
constructSelector ('-':s) = Ignore $ map T.pack $ split s
constructSelector s = Match $ map T.pack $ split s

split :: String -> [String]
split = splitOn '.'

splitOn :: Char -> String -> [String]
splitOn c s = go s []
  where
    go :: String -> String -> [String]
    go [] s = [s]
    go (sc:ss) acc | sc == c   = acc : go [] ss
                   | otherwise = go ss (acc ++ [sc])

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

matches :: [Text] -> [Text] -> Bool
matches ps s = s `isPrefixOf` ps

