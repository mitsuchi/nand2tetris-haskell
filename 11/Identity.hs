module Identity where

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    -- pure :: a -> p a
    pure a = Identity a
    -- <*> :: p (a->b) -> p a -> p b
    Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
    -- return :: a -> m a
    return = pure
    -- >>= :: m a -> (a -> m b) -> m b
    Identity a >>= f = f a
