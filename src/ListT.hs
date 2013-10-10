{-# LANGUAGE RankNTypes #-}
module ListT where

import Control.Monad
import Control.Arrow (first)

data ListT a m r
    = Pure r
    | M (m (ListT a m r))
    | Yield (ListT a m r) (m ()) a

instance Monad m => Monad (ListT a m) where
    return = Pure
    Pure r >>= f = f r
    Yield next done a >>= f = Yield (next >>= f) done a
    M m >>= f = M (liftM (>>= f) m)

newtype ConduitM i o m r = ConduitM
    { unConduitM :: forall u. ListT i m u -> ListT o m (ListT i m u, r)
    }

instance Monad m => Monad (ConduitM i o m) where
    return x = ConduitM $ \up -> Pure (up, x)
    ConduitM f >>= g = ConduitM $ \up -> do
        (up', a) <- f up
        unConduitM (g a) up'

fuse :: Monad m
     => ConduitM a b m ()
     -> ConduitM b c m r
     -> ConduitM a c m r
fuse (ConduitM up) (ConduitM down) = ConduitM $ liftM (first drain) . down . up

drain :: Monad m
      => ListT b m (ListT a m u, ())
      -> ListT a m u
drain (Pure (x, ())) = x
drain (M m) = M (liftM drain m)
drain (Yield next _ _) = drain next -- FIXME this is incredibly wasteful

draw :: Monad m
     => ListT a m r
     -> m (Maybe a, ListT a m r)
draw (Pure r) = return (Nothing, Pure r)
draw (M m) = m >>= draw
draw (Yield next done a) = return (Just a, next) -- FIXME lost a finalizer

yield :: Monad m => o -> ConduitM i o m ()
yield o = ConduitM $ \up -> Yield (Pure (up, ())) (return ()) o