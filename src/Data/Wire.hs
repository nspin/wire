module Data.Wire
    ( Wire
    , WireIn(..)
    , WireOut(..)
    , expect
    ) where

import Data.Wire.Get
import Data.Wire.Put

type Wire a = (WireIn a, WireOut a) 

class WireIn a where
    get :: Get a

class WireOut a where
    put :: Putter a

expect :: WireIn a => a -> Get a
expect a = do
    a' <- get a
    if a' == a then a' else fail "expect"
