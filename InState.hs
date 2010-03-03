{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module InState (InState, inState) where

import Data.IORef

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
