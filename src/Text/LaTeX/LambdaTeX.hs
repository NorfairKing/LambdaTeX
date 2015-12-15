{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Text.LaTeX.LambdaTeX (
      module Text.LaTeX.LambdaTeX
    , module Text.LaTeX.LambdaTeX.Types
    , module Text.LaTeX.LambdaTeX.Reference
    ) where

import           Control.Monad                        (when)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Reader                 (MonadReader (..), asks)
import           Control.Monad.RWS                    (MonadRWS (..), RWST (..),
                                                       runRWST)
import           Control.Monad.State                  (MonadState (..), gets,
                                                       modify)
import           Control.Monad.Trans                  (MonadTrans (..), lift)
import           Control.Monad.Writer                 (MonadWriter (..))

import           Data.List                            (isPrefixOf)
import           Data.Ord                             (comparing)
import           Data.Set                             (Set)
import qualified Data.Set                             as S
import           Data.Text                            (Text)
import qualified Data.Text.IO                         as T (putStrLn)

import           Text.LaTeX.Base                      (LaTeX, LaTeXT,
                                                       execLaTeXT)

import           Text.LaTeX.LambdaTeX.Reference
import           Text.LaTeX.LambdaTeX.Selection
import           Text.LaTeX.LambdaTeX.Selection.Types
import           Text.LaTeX.LambdaTeX.Types


note :: Monad m => Text -> ΛTeXT m () -> ΛTeXT m ()
note partname func = do
    pushCurrentPart partname

    part <- currentPart
    s <- isSelected

    when s func

    popCurrentPart


isSelected :: Monad m => ΛTeXT m Bool
isSelected = do
    part <- currentPart
    sels <- λasks configSelection
    return $ selects part sels

currentPart :: Monad m => ΛTeXT m Part
currentPart = λgets stateCurrentPart

pushCurrentPart :: Monad m => Text -> ΛTeXT m ()
pushCurrentPart partname = λmodify (\s -> s { stateCurrentPart = pushPart (stateCurrentPart s) partname})

popCurrentPart :: Monad m => ΛTeXT m ()
popCurrentPart = λmodify (\s -> s { stateCurrentPart = popPart $ stateCurrentPart s })


