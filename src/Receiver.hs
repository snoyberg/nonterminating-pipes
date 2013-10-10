module Receiver where

import Control.Monad

data Receiver i m r
    = Pure r
    | Await (i -> Receiver i m r) (Receiver i m r)
    | M (m (Receiver i m r))

instance Monad m => Monad (Receiver i m) where
    return = Pure
    Pure r >>= f = f r
    Await more none >>= f = Await (more >=> f) (none >>= f)
    M m >>= f = M (liftM (>>= f) m)

newtype ConduitM i o m r = ConduitM
    { unConduitM :: Receiver o m r -> Receiver i m (Receiver o m r)
    }

{-
instance Monad m => Monad (ConduitM (i o m)) where
    return
    -}