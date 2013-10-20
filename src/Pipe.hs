{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
module Pipe where

import Control.Monad

data Step i o d m r
    = Pure r
    | M (m (Step i o d m r))
    | Await (Maybe i -> Step i o d m r)
    | Yield (Maybe (d, [o]) -> Step i o d m r) (Maybe o)

instance Monad m => Monad (Step i o d m) where
    return = Pure
    Pure r >>= f = f r
    M m >>= f = M (liftM (>>= f) m)
    Await next >>= f = Await (next >=> f)
    Yield next o >>= f = Yield (next >=> f) o

newtype Pipe i o d t m r = Pipe
    { unPipe :: Maybe (d, [o]) -> [i] -> Step i o d m (Maybe (d, [o]), [i], Either t r)
    }

instance Monad m => Monad (Pipe i o d t m) where
    return x = Pipe $ \mdo is -> Pure (mdo, is, Right x)

    Pipe f >>= g = Pipe $ \mdo is -> do
        (mdo', is', etr) <- f mdo is
        case etr of
            Left t -> return (mdo', is', Left t)
            Right r -> unPipe (g r) mdo' is'

await :: Monad m => Pipe i o d t m (Maybe i)
await = Pipe $ \mdo is ->
    case is of
        [] -> Await $ \mi -> Pure (mdo, [], Right mi)
        i:is' -> Pure (mdo, is', Right $ Just i)

yield :: Monad m => o -> Pipe i o d t m ()
yield o = Pipe $ \_ is -> Yield (\mdo -> Pure (mdo, is, Right ())) (Just o)

yieldTerm :: Monad m => o -> Pipe i o d d m ()
yieldTerm o = Pipe $ \_ is -> Yield (\mdo ->
    case mdo of
        Nothing -> Pure (mdo, is, Right ())
        Just (d, _) -> Pure (mdo, is, Left d)
        ) (Just o)

empty :: Monad m => Pipe i o d t m (d, [o])
empty = Pipe $ \mdo0 is ->
    let loop (Just (d, os)) = Pure (Just (d, os), is, Right (d, os))
        loop Nothing = Yield loop Nothing
     in loop mdo0

leftover :: Monad m => i -> Pipe i o d t m ()
leftover i = Pipe $ \mdo is -> Pure (mdo, i:is, Right ())

leftovers :: Monad m => [i] -> Pipe i o d t m ()
leftovers is = Pipe $ \mdo is' -> Pure (mdo, is ++ is', Right ())

check :: Monad m => Pipe i o d t m (Maybe (d, [o]))
check = Pipe $ \mdo is -> Pure (mdo, is, Right mdo)

idPipe :: Monad m => Pipe i i r t m r
idPipe = do
    mdo <- check
    case mdo of
        Just (d, os) -> do
            leftovers os
            return d
        Nothing -> do
            mi <- await
            case mi of
                Nothing -> do
                    (d, os) <- empty
                    leftovers os
                    return d
                Just i -> do
                    yield i
                    idPipe

fuse :: Monad m
     => Pipe i j b t m a
     -> Pipe j k c b m b
     -> Pipe i k c t m a
fuse up0 (Pipe down0) =
    Pipe $ \mc is ->
        let up x = unPipe up0 x is
         in go up $ down0 mc []
  where
    go up1 (Pure (mc0, js, either id id -> b)) =
        closeDown mc0
      where
        closeDown (Just cks) = closeUp cks $ up1 $ Just (b, js)
        closeDown Nothing = Yield closeDown Nothing

        closeUp cks (Pure (_, is, a)) = Pure (Just cks, is, a)
        closeUp cks (M m) = M (liftM (closeUp cks) m)
        closeUp cks (Yield up _) = closeUp cks $ up $ Just (b, js) -- FIXME double leftovers
        closeUp cks (Await f) = Await (closeUp cks . f)
    go up (M m) = M (liftM (go up) m)
    go up1 (Await f) =
        goUp $ up1 Nothing
      where
        goUp (Pure (_, is, a)) = go (\mb -> Pure (mb, is, a)) (f Nothing)
        goUp (M m) = M (liftM goUp m)
        goUp (Yield up j) = go up (f j)
        goUp (Await up) = Await (goUp . up)
    go up (Yield down o) = Yield (go up . down) o

sourceList :: Monad m => [o] -> Pipe i o d t m d
sourceList [] = liftM fst empty
sourceList (o:os) = yield o >> sourceList os

sum' :: Monad m => Pipe Int o d t m Int
sum' =
    loop 0
  where
    loop total = do
        mi <- await
        case mi of
            Nothing -> return total
            Just i -> loop $! total + i

runPipe :: Monad m => Pipe i o () r m r -> m r
runPipe (Pipe p0) =
    go $ p0 (Just ((), [])) []
  where
    go (Pure (_, _, r)) = return $ either id id r
    go (M m) = m >>= go
    go (Await f) = go $ f Nothing
    go (Yield f _) = go $ f $ Just ((), [])

consume :: Monad m => Pipe i o d t m [i]
consume =
    loop id
  where
    loop front = await >>= maybe (return $ front []) (\i -> loop $ front . (i:))

take' :: Monad m => Int -> Pipe i i d t m ()
take' 0 = return ()
take' count = await >>= maybe (return ()) (\i -> yield i >> take' (count - 1))

main :: IO ()
main = do
    let src :: Pipe i Int d d IO d
        src = do
            mapM_ yieldTerm [4..]
            liftM fst empty
        conduit :: Pipe Int Int d d IO d
        conduit = do
            take' 7
            liftM fst empty
        sink :: Pipe Int o d t IO [Int]
        sink = do
            leftover (3 :: Int)
            idPipe `fuse` leftover 2
            leftover 1
            consume
    let pipeline = src `fuse` conduit `fuse` sink
    res <- runPipe pipeline
    print res