-- | ΛTeX utilities
module Text.LaTeX.LambdaTeX.Utils where

import           Control.Exception
import           System.Directory  (removeFile)
import           System.IO.Error   (isDoesNotExistError)

-- | Remove the given file if it exists
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

