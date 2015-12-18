module Text.LaTeX.LambdaTeX.Part where

import           Data.Text (Text)

newtype Part = Part { unPart :: [Text] }

emptyPart :: Part
emptyPart = Part { unPart = [] }

pushPart :: Part -> Text -> Part
pushPart p t = Part { unPart = unPart p ++ [t] }

popPart :: Part -> Part
popPart p = Part { unPart = init $ unPart p }

