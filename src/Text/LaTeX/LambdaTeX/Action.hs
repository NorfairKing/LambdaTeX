{-# LANGUAGE OverloadedStrings #-}

module Text.LaTeX.LambdaTeX.Action where

import Text.LaTeX.LambdaTeX.Types

-- | Register an IO action (with a given name) that needs to be completed before the pdf can be built.
--
-- Use this to generate and build external resources that are included with @includegraphics@ for example.
registerAction ::
       Monad m
    => String -- ^ Name of the job
    -> IO () -- ^ Job
    -> ΛTeXT m ()
registerAction name func = λtell $ mempty {outputActions = [(name, func)]}
