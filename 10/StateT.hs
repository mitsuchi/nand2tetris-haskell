module StateT where

import MonadTrans

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Monad m) => Functor (StateT s m) where
    -- fmap :: (a -> b) -> f a -> f b
    -- fmap :: (a -> b) -> StateT s m a -> StateT s m b
    -- fmap :: (a -> b) -> StateT (s -> m (a,s)) -> StateT (s -> m (b,s))
    fmap ab (StateT smas) = StateT $ \s -> do
        (a, s1) <- smas s
        pure (ab a, s1)

instance (Monad m) => Applicative (StateT s m) where
    -- pure :: a -> f a
    -- pure :: a -> StateT s m a
    -- pure :: a -> StateT (s -> m (a, s))
    pure a = StateT $ \s -> pure (a, s)
    -- <*> :: f (a->b) -> f a -> f b
    -- <*> :: StateT (s -> m (a->b, s))
    --        -> StateT (s -> m (a, s))
    --        -> StateT (s -> m (b, s))
    StateT smabs <*> StateT smas = StateT $ \s -> do
        (ab, s1) <- smabs s
        (a, s2) <- smas s1
        pure (ab a, s2)

instance (Monad m) => Monad (StateT s m) where
    return = pure
    -- >>= :: m a -> (a -> m b) -> m b
    -- >>= :: StateT (s -> m (a, s))
    --        -> (a -> StateT (s -> m (b, s)))
    --        -> StateT (s -> m (b, s))
    StateT smas >>= aSsmbs = StateT $ \s -> do
        (a, s1) <- smas s
        let StateT smbs = aSsmbs a
        (b, s2) <- smbs s1
        pure (b, s2)

instance MonadTrans (StateT s) where
    lift ma = StateT $ \s -> do
        a <- ma
        pure (a, s)