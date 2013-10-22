module AssocViolation where

import Pipe
import Control.Monad.Trans.Class
import Control.Monad

source :: Pipe i Int d t IO ()
source = do
    lift $ putStrLn "Starting source"
    loop [1..2]
  where
    loop [] = lift $ putStrLn "Source is empty"
    loop (i:is) = do
        lift $ putStrLn $ "Source is yielding: " ++ show i
        yield i
        x <- check
        case x of
            Nothing -> loop is
            Just _ -> lift $ putStrLn "Source: downstream terminated"

sink :: Pipe Int o d t IO ()
sink = do
    lift $ putStrLn "Awaiting in sink"
    mx <- await
    case mx of
        Nothing -> lift $ putStrLn "await is Nothing, exiting sink"
        Just x -> do
            lift $ putStrLn $ "sink received: " ++ show x
            sink

conduit1 :: Pipe Int Int d t IO ()
conduit1 = do
    lift $ putStrLn "Awaiting in conduit1"
    mx <- await
    case mx of
        Nothing -> lift $ putStrLn "await is Nothing, exiting conduit1"
        Just x -> do
            lift $ putStrLn $ "conduit1: yielding " ++ show x
            yield x
            my <- check
            case my of
                Nothing -> conduit1
                Just _ -> lift $ putStrLn "conduit1: downstream terminated, I'm terminating"

conduit2 :: Pipe Int Int d t IO ()
conduit2 = do
    lift $ putStrLn "Awaiting in conduit2"
    mx <- await
    case mx of
        Nothing -> lift $ putStrLn "await is Nothing, exiting conduit1"
        Just x -> do
            lift $ putStrLn $ "conduit2: yielding " ++ show x
            yield x
            lift $ putStrLn $ "conduit2: early termination"

conduit :: Pipe Int Int d t IO ()
conduit = do
    lift $ putStrLn "starting conduit"
    conduit1 >-> conduit2
    lift $ putStrLn "finishing conduit"

main :: IO ()
main = do
    putStrLn "Version 1"
    runPipe $ source >-> (conduit >-> sink)

    putStrLn "\nVersion 2"
    runPipe $ (source >-> conduit) >-> sink