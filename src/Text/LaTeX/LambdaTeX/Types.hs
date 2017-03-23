{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.LaTeX.LambdaTeX.Types
    ( module Text.LaTeX.LambdaTeX.Types
    , module Text.LaTeX.LambdaTeX.Part
    , module Text.LaTeX.LambdaTeX.Error
    , module Text.LaTeX.LambdaTeX.Package.Types
    , module Text.LaTeX.LambdaTeX.Reference.Types
    , module Text.LaTeX.LambdaTeX.Selection.Types
    , Text
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS (RWST(..))
import Control.Monad.Reader (MonadReader(..), asks)
import Control.Monad.State (MonadState(..), gets, modify)
import Control.Monad.Trans (MonadTrans(..), lift)
import Control.Monad.Writer (MonadWriter(..))

import Data.Set (Set)
import qualified Data.Set as S
import Data.String (IsString(..))
import Data.Text (Text)

import Text.LaTeX.Base (LaTeX, LaTeXT, runLaTeXT)
import Text.LaTeX.Base.Class (LaTeXC(..), fromLaTeX)
import Text.LaTeX.Base.Writer (extractLaTeX, textell)

import Text.LaTeX.Packages.AMSMath ()

import Text.LaTeX.LambdaTeX.Error
import Text.LaTeX.LambdaTeX.Package.Types
import Text.LaTeX.LambdaTeX.Part
import Text.LaTeX.LambdaTeX.Reference.Types
import Text.LaTeX.LambdaTeX.Selection.Types

-- * The ΛTeX Monad
type LambdaTeXT = ΛTeXT

type LambdaTeXT_ m = ΛTeXT_ m

type ΛTeXT_ m = ΛTeXT m ()

newtype ΛTeXT m a = ΛTeXT
    { unwrapΛTeXT :: LaTeXT (RWST ΛConfig ΛOutput ΛState m) a
    } deriving (Functor, Applicative, Monad)

-- TODO(kerckhove) Move and add amsmath dependency
instance (Monad m, a ~ ()) => Num (ΛTeXT m a) where
    (+) a b = ΛTeXT $ unwrapΛTeXT a + unwrapΛTeXT b
    (-) a b = ΛTeXT $ unwrapΛTeXT a - unwrapΛTeXT b
    (*) a b = ΛTeXT $ unwrapΛTeXT a * unwrapΛTeXT b
    negate = ΛTeXT . negate . unwrapΛTeXT
    fromInteger = fromLaTeX . fromInteger
    abs = ΛTeXT . abs . unwrapΛTeXT
    -- Non-defined methods
    signum = ΛTeXT . signum . unwrapΛTeXT

-- TODO(kerckhove) Move and add amsmath dependency
instance (Monad m, a ~ ()) => Fractional (ΛTeXT m a) where
    (/) a b = ΛTeXT $ unwrapΛTeXT a / unwrapΛTeXT b
    fromRational = fromLaTeX . fromRational

-- TODO(kerckhove) Also instantiate floating?
runΛTeX ::
       Monad m
    => ΛTeXT m a
    -> ΛConfig
    -> ΛState
    -> m ((a, LaTeX), ΛState, ΛOutput)
runΛTeX func = runRWST (runLaTeXT $ unwrapΛTeXT func)

instance MonadTrans ΛTeXT where
    lift = ΛTeXT . lift . lift

instance MonadIO m => MonadIO (ΛTeXT m) where
    liftIO = lift . liftIO

instance (Monad m, a ~ ()) => LaTeXC (ΛTeXT m a) where
    liftListL f xs = mapM extractΛLaTeX_ xs >>= λtextell . f

instance (Monad m, a ~ ()) => IsString (ΛTeXT m a) where
    fromString = λtextell . fromString

instance (Monad m, a ~ ()) => Monoid (ΛTeXT m a) where
    mempty = return ()
    mappend = (>>)

λtextell :: Monad m => LaTeX -> ΛTeXT m ()
λtextell = ΛTeXT . textell

λtell :: Monad m => ΛOutput -> ΛTeXT m ()
λtell = ΛTeXT . tell

λask :: Monad m => ΛTeXT m ΛConfig
λask = λasks id

λasks :: Monad m => (ΛConfig -> a) -> ΛTeXT m a
λasks = ΛTeXT . asks

λget :: Monad m => ΛTeXT m ΛState
λget = λgets id

λgets :: Monad m => (ΛState -> a) -> ΛTeXT m a
λgets = ΛTeXT . gets

λmodify :: Monad m => (ΛState -> ΛState) -> ΛTeXT m ()
λmodify = ΛTeXT . modify

-- | This function run a 'LaTeXT' computation,
-- lifting the result again in the monad.
extractΛLaTeX :: Monad m => ΛTeXT m a -> ΛTeXT m (a, LaTeX)
extractΛLaTeX = ΛTeXT . extractLaTeX . unwrapΛTeXT

-- | Like 'extractΛLaTeX' but without the result
extractΛLaTeX_ :: Monad m => ΛTeXT m a -> ΛTeXT m LaTeX
extractΛLaTeX_ = fmap snd . extractΛLaTeX

-- | Internal ΛTeXT configration
newtype ΛConfig = ΛConfig
    { configSelection :: Selection
    }

-- | Internal ΛTeXT configration output
data ΛOutput = ΛOutput
    { outputPackageDependencies :: Set PackageDep
    , outputExternalReferences :: Set Reference
    , outputLabelsMade :: Set Text
    , outputLabelsNeeded :: Set Text
    , outputActions :: [(String, IO ())]
    }

instance Monoid ΛOutput where
    mempty =
        ΛOutput
        { outputPackageDependencies = S.empty
        , outputExternalReferences = S.empty
        , outputLabelsMade = S.empty
        , outputLabelsNeeded = S.empty
        , outputActions = []
        }
    mappend o1 o2 =
        ΛOutput
        { outputPackageDependencies =
              S.union
                  (outputPackageDependencies o1)
                  (outputPackageDependencies o2)
        , outputExternalReferences =
              S.union
                  (outputExternalReferences o1)
                  (outputExternalReferences o2)
        , outputLabelsMade = S.union (outputLabelsMade o1) (outputLabelsMade o2)
        , outputLabelsNeeded =
              S.union (outputLabelsNeeded o1) (outputLabelsNeeded o2)
        , outputActions = outputActions o1 ++ outputActions o2
        }

-- | Internal ΛTeXT configration state
newtype ΛState = ΛState
    { stateCurrentPart :: Part
    }

-- Orphan Monad Transformer instances for LaTeXT
instance MonadReader r m => MonadReader r (LaTeXT m) where
    ask = lift ask
    local = local
    reader = lift . reader

instance MonadState s m => MonadState s (LaTeXT m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadWriter w m => MonadWriter w (LaTeXT m) where
    tell = lift . tell
    listen = listen
    pass = pass

-- Monad Transformer instances for ΛTeXT
instance MonadReader r m => MonadReader r (ΛTeXT m) where
    ask = lift ask
    local = local
    reader = lift . reader

instance MonadState s m => MonadState s (ΛTeXT m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadWriter w m => MonadWriter w (ΛTeXT m) where
    tell = lift . tell
    listen = listen
    pass = pass
