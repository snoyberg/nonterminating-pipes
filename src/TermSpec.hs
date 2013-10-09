module TermSpec where

import CoreSpec hiding (main)
import Term
import Test.Hspec
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad
import Prelude hiding (take, drop)

main :: IO ()
main = hspec $ do
    describe "summing" $ do
        let src = mapM_ yieldT [1..] >-> take 10 >> return Nothing
            sink = liftM Just (fold (+) 0)
            expected = sum [1..10] :: Int
            test name pipe = it name $ do
                res <- runPipeT pipe
                res `shouldBe` Just expected
        test "no identity" $ src >-> sink
        test "idPipe front" $ idPipe >-> src >-> sink
        test "idPipe middle" $ src >-> idPipe >-> sink
        test "idPipe end" $ src >-> sink >-> idPipe
        test "idPipe' front" $ idPipe' >-> src >-> sink
        test "idPipe' middle" $ src >-> idPipe' >-> sink
        test "idPipe' end" $ src >-> sink >-> idPipe'
        test "idPipeT front" $ idPipeT >-> src >-> sink
        test "idPipeT middle" $ src >-> idPipeT >-> sink
        --test "idPipeT end" $ src >-> sink >-> idPipeT
    describe "finalizer" $ do
        let src = yieldMany [1 :: Int ..] >> lift (put 1) >> empty
            sink = do
                drop 5
                take 5 >-> fold (+) 0
            expected = (sum [6..10], 1 :: Int)
        it "finalizer is run" $ do
            let res = flip runState 0 $ runPipe (src >-> sink)
            res `shouldBe` expected