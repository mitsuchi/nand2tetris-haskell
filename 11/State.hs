{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module State where

import MonadState

newtype State s a = State { runState :: s -> (a, s) }

execState (State f) s = fst $ f s

instance Functor (State s) where
    -- fmap :: (a -> b) -> f a -> f b
    -- fmap :: (a -> b) -> State s a -> State s b
    -- fmap :: (a -> b) -> State (s -> (a,s)) -> State (s -> (b,s))
    fmap ab (State sas) = State $ \s -> let
        (a, s1) = sas s
        b = ab a
        in (b, s1)

instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    -- <*> :: f (a->b) -> f a -> f b
    -- <*> :: State (s -> (a->b,s) -> State (s -> (a,s)) -> State (s->(b,s))
    State sabs <*> State sas = State $ \s -> let
        (a, s1) = sas s
        (ab, s2) = sabs s1
        b = ab a
        in (b, s2)

instance Monad (State s) where
    return = pure
    -- >>= :: m a -> (a -> m b) -> m b
    -- >>= :: State s a -> (a -> State s b) -> State s b
    -- >>= :: State (s -> (a,s)) -> (a -> State (s -> (b,s))) -> State (s->(b,s)))
    State sas >>= aSsbs = State $ \s -> let
        (a, s1) = sas s
        State sbs = aSsbs a
        (b, s2) = sbs s1
        in (b, s2)

instance MonadState s (State s) where
    -- get :: m s
    -- get :: State s s
    -- get :: State (s -> (s,s))
    get = State $ \s -> (s, s)

    -- put :: s -> m ()
    -- put :: s -> State s ()
    -- put :: s -> State (s -> ((), s))
    put s = State $ \_ -> ((), s)
    

