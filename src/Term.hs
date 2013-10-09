{-# OPTIONS_GHC -Wall #-}
module Term
    ( module Core
    , module Term
    ) where

import Core
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Control.Monad

type PipeT i o d t m r = Pipe i o d (EitherT t m) r

stop :: Monad m => t -> PipeT i o d t m r
stop = lift . left

emptyStop :: Monad m => PipeT i o d d m r
emptyStop = empty >>= stop

yieldT :: Monad m => o -> PipeT i o d d m ()
yieldT o = yield o >> check >>= maybe (return ()) stop

awaitT :: Monad m => PipeT i o d d m i
awaitT = await >>= maybe emptyStop return

fuseT :: Monad m
      => PipeT i j b t m a
      -> PipeT j k c t m b
      -> PipeT i k c t m a
fuseT = fuse

runPipeT :: Monad m
         => PipeT i o () r m r
         -> m r
runPipeT = liftM (either id id) . runEitherT . runPipe

idPipeT :: Monad m => PipeT i i r r m r
idPipeT = forever $ awaitT >>= yieldT