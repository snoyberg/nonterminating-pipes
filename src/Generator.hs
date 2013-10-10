{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
module Generator where

import Control.Monad
import Control.Arrow (first)
import Control.Monad.Trans.Class
import Data.Void

data Generator o m r
    = Yield (Generator o m r) o
    | Done r
    | M (m (Generator o m r))
    | Check (Generator o m r) (Generator o m r)

instance Monad m => Monad (Generator o m) where
    return = Done

    Done r >>= f = f r
    Yield next o >>= f = Yield (next >>= f) o
    M m >>= f = M (liftM (>>= f) m)
    Check next done >>= f = Check (next >>= f) (done >>= f)

instance MonadTrans (Generator o) where
    lift = M . liftM Done

draw :: Monad m
     => Generator o m r
     -> m (Generator o m r, Maybe o)
draw (Yield gen o) = return (gen, Just o)
draw (Done r) = return (Done r, Nothing)
draw (M m) = m >>= draw
draw (Check next _) = draw next

closeGen :: Monad m => Generator o m r -> m r
closeGen =
    go
  where
    go (Yield gen _) = go gen
    go (Done r) = return r
    go (M m) = m >>= go
    go (Check _ done) = go done

newtype ConduitM i o m r = ConduitM
    { unConduitM :: forall u.
                    Generator i m u
                 -> Generator o m (Generator i m u, r)
    }

runConduitM :: Monad m => ConduitM () Void m r -> m r
runConduitM (ConduitM f) = do
    (gen, r) <- closeGen $ f $ Done ()
    closeGen gen
    return r

instance Monad m => Monad (ConduitM i o m) where
    return x = ConduitM $ \up -> Done (up, x)
    ConduitM f >>= g = ConduitM $ \up -> do
        (up', a) <- f up
        unConduitM (g a) up'

instance MonadTrans (ConduitM i o) where
    lift m = ConduitM $ \up -> M $ do
        x <- m
        return $ Done (up, x)

yield :: Monad m => o -> ConduitM i o m ()
yield o = ConduitM $ \up -> Yield (Done (up, ())) o

await :: Monad m => ConduitM i o m (Maybe i)
await = ConduitM $ M . liftM Done . draw

check :: Monad m => ConduitM i o m Bool
check = ConduitM $ \up -> Check (Done (up, True)) (Done (up, False))

leftover :: Monad m => i -> ConduitM i o m ()
leftover i = ConduitM $ \up -> Done (Yield up i, ())

fuse :: Monad m
     => ConduitM a b m ()
     -> ConduitM b c m r
     -> ConduitM a c m r
fuse (ConduitM up) (ConduitM down) = ConduitM $ liftM (first drain) . down . up

drain :: Monad m
      => Generator b m (Generator a m u, ())
      -> Generator a m u
drain (Yield g _b) = drain g
drain (M m) = M (liftM drain m)
drain (Done (gen, ())) = gen
drain (Check _ done) = drain done

---

main :: IO ()
main = do
    let src = do
            let go i = say ("yielding: " ++ show i) >> yield i
            mapM_ go [1..10 :: Int]
            say "Finalize src"
        conduit =
            loop (5 :: Int)
          where
            loop 0 = say "finishing conduit"
            loop i = await >>= maybe (say "conduit: early term") (\x -> yield x >> loop (i - 1))
        sink =
            loop 0
          where
            loop r = await >>= maybe (say "Finalize sink" >> return r) (\i -> loop $! r + i)
        say :: String -> ConduitM i o IO ()
        say = lift . putStrLn
    runConduitM (src `fuse` conduit `fuse` sink) >>= print