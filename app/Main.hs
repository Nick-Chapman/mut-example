{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Main(main) where

import Data.Array.MArray (newArray,readArray,writeArray) -- very very polymorphic mutable array ops
import Data.Array.IO (IOArray) -- mutable arrays in the IO monad
import Data.Array.ST (STArray) -- mutable arrays in the ST monad
import Control.Monad.ST (ST,runST)

main :: IO ()
main = do
    putStrLn "--fibA..(hang on).."; print $ fibA 40
    putStrLn "--fibB.."; fibB 10 >>= print
    putStrLn "--fib3.."; print (fibC 50)


-- | original, unmemoized fib
fibA :: Int -> Int
fibA n = if n <= 1 then 1 else fibA (n-2) + fibA (n-1)


-- | fib memoized using arrays within the IO monad.
-- | Being in the IO monad allows us to intermingle arbitary IO ops (e.g. print) with the array ops.
-- | But the price is the return type is `IO Int`.
fibB :: Int -> IO Int
fibB n = do
    a <- newArray (2,n) Nothing
    fibMemIO a n

fibMemIO :: IOArray Int (Maybe Int) -> Int -> IO Int
fibMemIO a n = do
    if n <= 1 then return 1 else do
        readArray a n >>= \case
            Just m -> do
                putStrLn $ "using memo: fib(" <> show n <> ") -> " <> show m
                return m
            Nothing -> do
                putStrLn $ "computing: fib(" <> show n <> ")..."
                x <- fibMemIO a (n-2)
                y <- fibMemIO a (n-1)
                let res = x + y
                putStrLn $ "computed: fib (" <> show n <> ") = " <> show res
                writeArray a n (Just res)
                return res


-- | fib memoized using arrays within the ST monad.
-- | Being in the ST monad means we cannot print.
-- | But `runST` allow us to hide our use of mutable arrays. Note return type is `Int`.
fibC :: Int -> Int
fibC n = runST $ do
    a <- newArray (2,n) Nothing
    fibMemST a n

fibMemST :: STArray q Int (Maybe Int) -> Int -> ST q Int
fibMemST a n = do
    if n <= 1 then return 1 else do
        readArray a n >>= \case
            Just m -> do return m
            Nothing -> do
                x <- fibMemST a (n-2)
                y <- fibMemST a (n-1)
                let res = x + y
                writeArray a n (Just res)
                return res
