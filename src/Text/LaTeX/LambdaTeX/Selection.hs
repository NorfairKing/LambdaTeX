module Text.LaTeX.LambdaTeX.Selection where

import Control.Monad (when)

import qualified Data.Text as T

import Text.LaTeX.LambdaTeX.Selection.Internal
import Text.LaTeX.LambdaTeX.Selection.Types
import Text.LaTeX.LambdaTeX.Types

-- * Making selections
-- | Construct a selection from a space-separated list of strings representing selectors
--
-- > constructSelection = map constructSelector . words
constructSelection :: String -> Selection
constructSelection = map constructSelector . words

-- | Construct a selector from a string
--
-- >>> constructSelector "all"
-- > All
--
-- >>> constructSelector "mySection.mySubsection"
-- > Match ["mySection", "mySubsection"]
--
-- >>> constructSelector "+mySection.mySubsection"
-- > Match ["mySection", "mySubsection"]
--
-- >>> constructSelector "-mySection.mySubsection"
-- > Ignore ["mySection", "mySubsection"]
constructSelector :: String -> Selector
constructSelector "all" = All
constructSelector ('+':s) = Match $ map T.pack $ split s
constructSelector ('-':s) = Ignore $ map T.pack $ split s
constructSelector s = Match $ map T.pack $ split s

-- * Using selections
-- | Declare a sub-part of the document with a name.
--   This allows you to use the subset-selection feature later.
note :: Monad m => Text -> ΛTeXT m () -> ΛTeXT m ()
note partname func =
    inPart partname $ do
        s <- isSelected
        when s func
