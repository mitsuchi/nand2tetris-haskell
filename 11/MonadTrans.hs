module MonadTrans where

-- type constructor t is a MonadTrans class
-- t needs 2 type parameter :: * -> * -> *
class MonadTrans t where
    -- lift a computation from the argument monad to the constructed monad
    lift :: (Monad m) => m a -> t m a
    
    