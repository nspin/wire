module Data.Wire.Get
    (
      module Data.Wire.Get.Monad

    , satisfy
    , expect

    , getBytesStrict
    , getBytesLazy

    -- * Debugging 'Get's
    , formatFailure

    , (<&>)
    , (<**>)
    ) where

import Data.Wire.Get.Monad

import Data.Functor
import Control.Applicative

import Data.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

expect :: MonadGet m => Eq a => a -> m a -> m a
expect a = satisfy (== a)

satisfy :: MonadGet m => (a -> Bool) -> m a -> m a
satisfy f g = do
    a <- g
    if f a then return a else fail "satisfy"

getBytesStrict :: MonadGet m => Int -> m B.ByteString
getBytesStrict n = L.toStrict <$> getBytesLazy n

getBytesLazy :: MonadGet m => Int -> m L.ByteString
getBytesLazy n = toLazyByteString <$> getBytes n

-- Debugging 'Get's

formatFailure :: Failure -> String
formatFailure (Failure b ctx msg) = unlines $
    [ "Failure: " <> msg
    , "Buffer: " <> buf
    ] <> map ("In: " <>) ctx
  where
    buf = if B.null no then show yes else show yes <> " (" <> show (B.length no) <> " not shown)"
    (yes, no) = B.splitAt 8 b
