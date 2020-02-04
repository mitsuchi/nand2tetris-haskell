{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module MonadState where

class Monad m => MonadState s m | m -> s where
    get :: m s
    put :: s -> m ()

