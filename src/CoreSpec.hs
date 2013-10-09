module CoreSpec where

import Core
import Test.Hspec
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Prelude hiding (take, drop)

fold :: Monad m => (r -> i -> r) -> r -> Pipe i o d m r
fold f =
    loop
  where
    loop r = await >>= maybe (return r) (\i -> loop $! f r i)

yieldMany :: Monad m => [o] -> Pipe i o d m ()
yieldMany [] = return ()
yieldMany (o:os) = check >>= maybe (yield o >> yieldMany os) (const $ return ())

(>->) :: Monad m
     => Pipe i j b m a
     -> Pipe j k c m b
     -> Pipe i k c m a
(>->) = fuse

take :: Monad m => Int -> Pipe i i d m d
take 0 = empty
take count = await >>= maybe empty (\i -> yield i >> take (count - 1))

drop :: Monad m => Int -> Pipe i o d m ()
drop count = take count >-> return ()

main :: IO ()
main = hspec $ do
    describe "summing" $ do
        let src = yieldMany [1..10 :: Int] >> empty
            sink = fold (+) 0
            expected = sum [1..10]
            test name pipe = it name $ do
                res <- runPipe pipe
                res `shouldBe` expected
        test "no identity" $ src >-> sink
        test "idPipe front" $ idPipe >-> src >-> sink
        test "idPipe' front" $ idPipe' >-> src >-> sink
        test "idPipe middle" $ src >-> idPipe >-> sink
        test "idPipe' middle" $ src >-> idPipe' >-> sink
        test "idPipe end" $ src >-> sink >-> idPipe
        test "idPipe' end" $ src >-> sink >-> idPipe'
    describe "finalizer" $ do
        let src = yieldMany [1 :: Int ..] >> lift (put 1) >> empty
            sink = do
                drop 5
                take 5 >-> fold (+) 0
            expected = (sum [6..10], 1 :: Int)
        it "finalizer is run" $ do
            let res = flip runState 0 $ runPipe (src >-> sink)
            res `shouldBe` expected