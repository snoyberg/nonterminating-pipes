{-# OPTIONS_GHC -Wall #-}
module Core where

import Control.Monad
import Control.Applicative hiding (empty)
import Control.Monad.Trans.Class
import Control.Monad.Morph

data Step i o d m r
    = Pure r
    | M (m (Step i o d m r))
    | Await (Maybe i -> Step i o d m r)
    | Yield (Pipe i o d m r) (Maybe o)

newtype Pipe i o d m r = Pipe
    { unPipe :: Maybe d -> Step i o d m r
    }

instance Monad m => Functor (Pipe i o d m) where
    fmap = liftM
instance Monad m => Applicative (Pipe i o d m) where
    pure = return
    (<*>) = ap
instance Monad m => Monad (Pipe i o d m) where
    return = Pipe . const . Pure

    Pipe f >>= g =
        Pipe $ \md -> go md $ f md
      where
        go md (Pure r) = unPipe (g r) md
        go md (M m) = M (liftM (go md) m)
        go md (Await next) = Await (go md . next)
        go _ (Yield next o) = Yield (next >>= g) o
instance MonadTrans (Pipe i o d) where
    lift m = Pipe $ \_ -> M (liftM Pure m)

instance MFunctor (Step i o d) where
    hoist _ (Pure r) = Pure r
    hoist f (M m) = M (f (liftM (hoist f) m))
    hoist f (Await next) = Await (hoist f . next)
    hoist f (Yield next o) = Yield (hoist f next) o

instance MFunctor (Pipe i o d) where
    hoist f (Pipe p) = Pipe (hoist f . p)

fuse :: Monad m
     => Pipe i j b m a
     -> Pipe j k c m b
     -> Pipe i k c m a
fuse up (Pipe down) = Pipe $ \mc -> fuseS mc up (down mc)

fuseS :: Monad m
      => Maybe c
      -> Pipe i j b m a
      -> Step j k c m b
      -> Step i k c m a
fuseS (Just _) (Pipe up0) (Pure b) =
    close (up0 (Just b))
  where
    close (Pure a) = Pure a
    close (M m) = M (liftM close m)
    close (Yield (Pipe up) _) = close (up (Just b))
    close (Await next) = Await (close . next)
-- Ensure that downstream closes first
fuseS Nothing up (Pure b) = Yield (fuse up (return b)) Nothing
fuseS mc up (M down) = M (liftM (fuseS mc up) down)
fuseS mc (Pipe up0) (Await down) =
    go (up0 Nothing)
  where
    go (Pure a) = fuseS mc (Pipe $ const $ Pure a) (down Nothing)
    go (M m) = M (liftM go m)
    go (Yield up mj) = fuseS mc up (down mj)
    go (Await up) = Await (go . up)
fuseS _ up (Yield down mk) = Yield (fuse up down) mk

runPipe :: Monad m => Pipe i o () m r -> m r
runPipe (Pipe f) = runStep (f (Just ()))

runStep :: Monad m => Step i o () m r -> m r
runStep (Pure r) = return r
runStep (M m) = m >>= runStep
runStep (Await f) = runStep (f Nothing)
runStep (Yield pipe _) = runPipe pipe

idPipe :: Monad m => Pipe i i r m r
idPipe = Pipe $ maybe (Await (Yield idPipe)) Pure

await :: Monad m => Pipe i o d m (Maybe i)
await = Pipe $ const $ Await Pure

yield :: Monad m => o -> Pipe i o d m ()
yield = Pipe . const . Yield (return ()) . Just

check :: Monad m => Pipe i o d m (Maybe d)
check = Pipe Pure

empty :: Monad m => Pipe i o d m d
empty = Pipe $ maybe (Yield empty Nothing) Pure

idPipe' :: Monad m => Pipe i i r m r
idPipe' = check >>= maybe (await >>= maybe empty (yield >=> const idPipe')) return