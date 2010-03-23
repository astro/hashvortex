{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module InState (InState, inState, refInStateT) where

import Data.IORef
import Control.Monad.State

class InState r f where
    inState :: f -> r -> IO ()

instance InState (IORef a) (a -> IO a) where
    inState f r = readIORef r >>=
                  f >>=
                  writeIORef r

instance InState (IORef a) (a -> IO ()) where
    inState f r = readIORef r >>= f

instance InState (IORef a) (a -> a) where
    inState = flip modifyIORef


refInStateT :: IORef s -> StateT s IO a -> IO a
refInStateT ref f
    = do s <- readIORef ref
         (a, s') <- runStateT f s
         writeIORef ref s'
         return a

