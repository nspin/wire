{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}

module Data.Wire.Get.ByteString.Internal
    ( Get(..)
    , Partial(..)
    , PartialResult(..)
    , runGet
    , runGetOn
    , runGetOnMaybe
    ) where

import Data.Wire.Get.Monad

import Prelude hiding (fail)
import Control.Monad.Fail
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.Word (Word8)
import Foreign.Storable
import qualified Data.ByteString as B
import qualified Control.Monad

data Get a = Get Int (forall r. ByteString -> [String] -> SuccessK a r -> PartialResult r)

type SuccessK a r = ByteString -> a -> PartialResult r

data PartialResult r =
      FailureR Failure
    | PartialR (Partial r)
    | SuccessR r
    deriving (Show, Functor)

data Partial r = Partial [String] Int (ByteString -> PartialResult r)

instance Show (Partial r) where
    show (Partial ctx n k) = "Partial _ " ++ show n ++ " _"

instance Functor Partial where
    fmap f (Partial ctx n k) = Partial ctx n (fmap (fmap f) k)

instance Functor Get where
    fmap f (Get n p) = Get n $ \b0 ctx ks ->
                              p b0 ctx $ \b1 a -> ks b1 (f a)

instance Applicative Get where
    {-# INLINE pure #-}
    pure a = Get 0 $ \b0 ctx ks -> ks b0 a

    {-# INLINE (<*>) #-}
    Get fn fp <*> Get xn xp = Get (fn + xn) $ \b0 ctx ks ->
                                            fp b0 ctx $ \b1 f ->
                                            xp b1 ctx $ \b2 x -> ks b2 (f x)

    {-# INLINE (*>) #-}
    Get xn xp *> Get yn yp = Get (xn + yn) $ \b0 ctx ks ->
                                           xp b0 ctx $ \b1 _ -> yp b1 ctx ks

instance Monad Get where

    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    Get xn xp >>= f = Get xn $ \b0 ctx ks ->
                             xp b0 ctx $ \b1 a ->
                                let Get yn yp = f a
                                in PartialR (Partial ctx yn (\b2 -> yp b2 ctx ks))

    {-# INLINE (>>) #-}
    (>>) = (*>)

    {-# INLINE fail #-}
    fail = fail

instance MonadFail Get where
    {-# INLINE fail #-}
    fail msg = Get 0 $ \b0 ctx _ -> FailureR (Failure b0 ctx msg)


successK :: SuccessK r r
successK b = if B.null b then SuccessR else error "Data.Wire.Get.Internal: user error: leftover input"

runGet :: Get a -> Partial a
runGet (Get 0 _) = error "Data.Message.Get.Internal: user error: cannot run empty parser"
runGet (Get n p) = Partial [] n (\b -> p b [] successK)

runGetOn :: Monad m => (Int -> m ByteString) -> Get a -> m (Result a)
runGetOn m = go . runGet
  where
    go (Partial ctx n ks) = do
        b <- m n
        if B.null b
            then return (Left (notEnoughInput ctx))
            else case ks b of
                FailureR fl -> return (Left fl)
                SuccessR a -> return (Right a)
                PartialR pt -> go pt

runGetOnMaybe :: Monad m => (Int -> m (Maybe ByteString)) -> Get a -> m (Result a)
runGetOnMaybe m = go . runGet
  where
    go (Partial ctx n ks) = do
        mb <- m n
        case mb of
            Nothing -> return (Left (notEnoughInput ctx))
            Just b -> case ks b of
                FailureR fl -> return (Left fl)
                SuccessR a -> return (Right a)
                PartialR pt -> go pt


instance MonadGet Get where

    {-# INLINE label #-}
    label :: String -> Get a -> Get a
    label l (Get n p) = Get n (\b ctx ks -> p b (l:ctx) ks)

    {-# INLINE skip #-}
    skip :: Int -> Get ()
    skip n = Get n (go n)
      where
        go n0 b0 ctx ks =
            let n1 = n0 - B.length b0
            in case compare n1 0 of
                EQ -> ks mempty ()
                LT -> ks (B.drop n0 b0) ()
                GT -> PartialR (Partial ctx n1 (\b1 -> go n1 b1 ctx ks))

    {-# INLINE getByte #-}
    getByte :: Get Word8
    getByte = Get 1 go
      where
        go b0 ctx ks = case B.uncons b0 of
            Just (w, b1) -> ks b1 w
            Nothing -> PartialR (Partial ctx 1 (\b1 -> go b1 ctx ks))

    {-# INLINE getBytes #-}
    getBytes :: Int -> Get Builder
    getBytes n = Get n (go mempty n)
      where
        go acc n0 b0 ctx ks =
            let n1 = n0 - B.length b0
            in case compare n1 0 of
                EQ -> ks mempty (acc <> byteString b0)
                LT -> uncurry ks $ byteString <$> B.splitAt n0 b0
                GT -> PartialR (Partial ctx n1 (\b1 -> go (acc <> byteString b0) n1 b1 ctx ks))

    {-# INLINE getStorable #-}
    getStorable :: Storable a => Get a
    getStorable = undefined

    {-# INLINE expectBytes #-}
    expectBytes :: ByteString -> Get ()
    expectBytes b = Get (B.length b) (go b)
      where
        go br b0 ctx ks =
            let n1 = B.length br - B.length b0
            in case compare n1 0 of
                EQ -> ks mempty ()
                LT -> case B.stripPrefix br b0 of
                    Nothing -> FailureR (Failure b ctx "match bytes")
                    Just b1 -> ks b1 ()
                GT -> case B.stripPrefix b0 br of
                    Nothing -> FailureR (Failure b ctx "match bytes")
                    Just br' -> PartialR (Partial ctx n1 (\b1 -> go br' b1 ctx ks))

    {-# INLINE match #-}
    match :: Get a -> Get (Builder, a)
    match g = undefined
