module Data.Wire.Get
    ( Get
    , Result
    , Failure(..)
    , Partial(..)
    , PartialResult(..)
    , runGetPartial
    , runGet
    , runGetMaybe

    , label
    , skip

    , getByte
    , getBytes
    , matchByte
    , matchBytes

    -- * For debugging 'Get's
    , formatFailure
    , runGetStrict
    , runGetLazy
    ) where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.Wire.Get.Internal
import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

matchByte :: Word8 -> Get ()
matchByte w = do
    w' <- getByte
    when (w' /= w) . fail $ "match byte"

formatFailure :: Failure -> String
formatFailure (Failure b ctx msg) = unlines $
    [ "Failure: " <> msg
    , "Buffer: " <> buf
    ] <> map ("In: " <>) ctx
  where
    buf = if B.null no then show yes else show yes <> " (" <> show (B.length no) <> " not shown)"
    (yes, no) = B.splitAt 8 b

getBytesStrict :: Int -> Get ByteString
getBytesStrict n = L.toStrict <$> getBytesLazy n

getBytesLazy :: Int -> Get L.ByteString
getBytesLazy n = toLazyByteString <$> getBytes n

runGetStrict :: Get a -> ByteString -> (Result a, ByteString)
runGetStrict g = runState (runGet g m)
  where
    m n = do
        st <- get
        if B.null st
            then return mempty
            else state (B.splitAt n)

runGetLazy :: Get a -> L.ByteString -> (Result a, L.ByteString)
runGetLazy g lbs = L.fromChunks . uncurry (:) <$> runState (runGet g m) (mempty, L.toChunks lbs)
  where
    m n = do
        (sst, lst) <- get
        case (B.null sst, lst) of
            (True, []) -> return mempty
            (True, l:ls) -> put (l, ls) >> m n
            (False, _) ->
                let (s, sst') = B.splitAt n sst
                in s <$ put (sst', lst)
