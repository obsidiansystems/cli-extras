{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cli.Extras.SubExcept where

import Control.Lens (Prism', preview, review)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Reader
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.Log

-- | Wrap a Prism' in a newtype to avoid impredicativity problems
newtype WrappedPrism' a b = WrappedPrism' { unWrappedPrism' :: Prism' a b }

newtype SubExceptT e eSub m a = SubExceptT { unSubExceptT :: ReaderT (WrappedPrism' e eSub) m a }
  deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadMask, MonadIO, MonadFail)

deriving instance MonadLog o m => MonadLog o (SubExceptT e eSub m)

instance MonadTrans (SubExceptT e eSub) where
  lift = SubExceptT . lift

instance MonadError e m => MonadError eSub (SubExceptT e eSub m) where
  throwError e = SubExceptT $ do
    WrappedPrism' p <- ask
    throwError $ review p e
  catchError a h = SubExceptT $ do
    WrappedPrism' p <- ask
    lift $ catchError (runSubExceptT p a) $ \e -> case preview p e of
      Nothing -> throwError e
      Just eSub -> runSubExceptT p $ h eSub

runSubExceptT :: Prism' e eSub -> SubExceptT e eSub m a -> m a
runSubExceptT p a = runReaderT (unSubExceptT a) (WrappedPrism' p)
