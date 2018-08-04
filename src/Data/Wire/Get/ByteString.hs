module Data.Wire.Get.ByteString
    ( Get
    , Partial(..)
    , PartialResult(..)
    , runGet
    , runGetOn
    , runGetOnMaybe
    -- * Debugging 'Get's
    , runGetStrict
    , runGetLazy
    ) where

import Data.Wire.Get
import Data.Wire.Get.ByteString.Internal
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

-- Debugging 'Get's

runGetStrict :: Get a -> B.ByteString -> (Result a, B.ByteString)
runGetStrict = S.runState . runGetOn m
  where
    m n = do
        st <- S.get
        if B.null st
            then return mempty
            else S.state (B.splitAt n)

runGetLazy :: Get a -> L.ByteString -> (Result a, L.ByteString)
runGetLazy g lbs = L.fromChunks . uncurry (:) <$> S.runState (runGetOn m g) (mempty, L.toChunks lbs)
  where
    m n = do
        (sst, lst) <- S.get
        case (B.null sst, lst) of
            (True, []) -> return mempty
            (True, l:ls) -> S.put (l, ls) >> m n
            (False, _) ->
                let (s, sst') = B.splitAt n sst
                in s <$ S.put (sst', lst)
