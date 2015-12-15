{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Text.LaTeX.LambdaTeX.Types (
      module Text.LaTeX.LambdaTeX.Types

    , Text
    ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
import           Data.Monoid
#endif

import           Control.Monad                        (liftM, when)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Reader                 (MonadReader (..),
                                                       ReaderT (..), asks)
import           Control.Monad.RWS                    (RWST (..))
import           Control.Monad.State                  (MonadState (..),
                                                       StateT (..), gets,
                                                       modify)
import           Control.Monad.Trans                  (MonadTrans (..), lift)
import           Control.Monad.Writer                 (MonadWriter (..),
                                                       WriterT (..))
import           Data.Functor.Identity

import           Data.List                            (isPrefixOf)
import           Data.Ord                             (comparing)
import           Data.Set                             (Set)
import qualified Data.Set                             as S
import           Data.String                          (IsString (..))
import           Data.Text                            (Text)
import qualified Data.Text.IO                         as T (putStrLn)

import           Text.LaTeX.Base                      (LaTeX, LaTeXT,
                                                       execLaTeXT, runLaTeXT)
import           Text.LaTeX.Base.Class                (LaTeXC (..))
import           Text.LaTeX.Base.Writer               (extractLaTeX,
                                                       extractLaTeX_, textell)

import           Text.LaTeX.LambdaTeX.Package.Types
import           Text.LaTeX.LambdaTeX.Reference.Types
import           Text.LaTeX.LambdaTeX.Selection.Types

-- * The ΛTeX Monad
type LambdaTeXT = ΛTeXT
type LambdaTeXT_ m = ΛTeXT_ m

type ΛTeXT_ m = ΛTeXT m ()
newtype ΛTeXT m a =
    ΛTeXT { unwrapΛTeXT :: RWST ΛConfig ΛOutput ΛState (LaTeXT m) a }

runΛTeX' :: Monad m => ΛTeXT m a -> ΛConfig -> ΛState -> m ((a, ΛState, ΛOutput), LaTeX)
runΛTeX' lt conf state = runLaTeXT (runRWST (unwrapΛTeXT lt) conf state)

instance Functor f => Functor (ΛTeXT f) where
    fmap f = ΛTeXT . fmap f . unwrapΛTeXT

instance (Functor f, Applicative f, Monad f) => Applicative (ΛTeXT f) where
    pure = ΛTeXT . pure
    (ΛTeXT f) <*> (ΛTeXT x) = ΛTeXT $ f <*> x

instance Monad m => Monad (ΛTeXT m) where
    return = ΛTeXT . return
    (ΛTeXT c) >>= f = ΛTeXT $ do
        a <- c
        let ΛTeXT c' = f a
        c'
    fail = return . error


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
λtextell = ΛTeXT . lift . textell

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
extractΛLaTeX :: Monad m => ΛTeXT m a -> ΛTeXT m (a,LaTeX)
extractΛLaTeX func = ΛTeXT $ do
    c <- ask
    s <- get

    let mfunc = unwrapΛTeXT func
    -- let doIt :: LaTeXT m (a, ΛState, ΛOutput)
        doIt = runRWST mfunc c s
    -- let getLaTeX :: LaTeXT m ((a, ΛState, ΛOutput), LaTeX)
        getLaTeX = extractLaTeX doIt
    ((result, state, output), latex) <- lift getLaTeX

    tell output
    put state
    return (result, latex)

extractΛLaTeX_ :: Monad m => ΛTeXT m a -> ΛTeXT m LaTeX
extractΛLaTeX_ = liftM snd . extractΛLaTeX

data ΛConfig    = Config {
      configSelection :: Selection
    }
data ΛOutput    = Output {
      outputPackageDependencies :: Set PackageDep
    , outputExternalReferences  :: Set Reference
    }

instance Monoid ΛOutput where
    mempty = Output {
            outputPackageDependencies   = S.empty
          , outputExternalReferences    = S.empty
        }

    mappend o1 o2 = Output {
            outputPackageDependencies =
                S.union
                    (outputPackageDependencies o1)
                    (outputPackageDependencies o2)
          , outputExternalReferences =
                S.union
                    (outputExternalReferences o1)
                    (outputExternalReferences o2)
        }

data ΛState = State {
      stateCurrentPart :: Part
    }

-- ** Part (Subset of notes)
newtype Part = Part { unPart :: [Text] }

pushPart :: Part -> Text -> Part
pushPart p t = Part { unPart = unPart p ++ [t] }

popPart :: Part -> Part
popPart p = Part { unPart = init $ unPart p }

-- * Orphan Monad Transformer instances for LaTeXT
instance MonadReader r m => MonadReader r (LaTeXT m) where
    ask   = lift ask
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


-- * Monad Transformer instances for ΛTeXT
instance MonadReader r m => MonadReader r (ΛTeXT m) where
    ask   = lift ask
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

