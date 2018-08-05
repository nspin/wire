module Data.Wire.Get.Monad
    ( MonadGet(..)
    , Result
    , Failure(..)
    , notEnoughInput
    ) where

import Control.Monad.Fail
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.Word (Word8)
import Foreign.Storable

class (Monad m, MonadFail m) => MonadGet m where
    label :: String -> m a -> m a
    skip :: Int -> m ()
    getByte :: m Word8
    getBytes :: Int -> m Builder
    getStorable :: Storable a => m a
    expectBytes :: ByteString -> m ()
    match :: m a -> m (Builder, a)

type Result r = Either Failure r

data Failure = Failure ByteString [String] String
    deriving (Eq)

instance Show Failure where
    show (Failure b ctx msg) = "Failure _ " ++ show ctx ++ " " ++ show msg

notEnoughInput :: [String] -> Failure
notEnoughInput ctx = Failure mempty ctx "not enough input"
