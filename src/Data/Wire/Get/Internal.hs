{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Wire.Get.Internal
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
    , matchBytes
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.Word (Word8)
import qualified Control.Monad.Fail as F
import qualified Data.ByteString as B


data Get a = Get Int (forall r. ByteString -> [String] -> SuccessK a r -> PartialResult r)

data PartialResult r =
      FailureR Failure
    | PartialR (Partial r)
    | SuccessR r
    deriving (Show, Functor)

type Result r = Either Failure r

data Failure = Failure ByteString [String] String
data Partial r = Partial Int (ByteString -> PartialResult r)

type SuccessK a r = ByteString -> a -> PartialResult r

instance Show Failure where
    show (Failure b ctx msg) = "Failure _ " ++ show ctx ++ " " ++ show msg -- TODO

instance Show (Partial r) where
    show (Partial n k) = "Partial " ++ show n ++ " _" -- TODO

instance Functor Partial where
    fmap f (Partial n k) = Partial n (fmap (fmap f) k)

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
                                in PartialR (Partial yn (\b2 -> yp b2 ctx ks))

    {-# INLINE (>>) #-}
    (>>) = (*>)

    {-# INLINE fail #-}
    fail = F.fail

instance F.MonadFail Get where
    {-# INLINE fail #-}
    fail msg = Get 0 $ \b0 ctx _ -> FailureR (Failure b0 ctx msg)


successK :: SuccessK r r
successK b = if B.null b then SuccessR else error "Data.Wire.Get.Internal: user error: leftover input"

runGetPartial :: Get a -> Partial a
runGetPartial (Get 0 _) = error "Data.Message.Get.Internal: user error: cannot run empty parser"
runGetPartial (Get n p) = Partial n (\b -> p b [] successK)

runGet :: Monad m => Get a -> (Int -> m ByteString) -> m (Result a)
runGet g m = go (runGetPartial g)
  where
    go (Partial n ks) = do
        b <- m n
        if B.null b
            then return (Left (notEnoughInput [])) -- TODO where is the ctx
            else case ks b of
                FailureR fl -> return (Left fl)
                SuccessR a -> return (Right a)
                PartialR pt -> go pt

runGetMaybe :: Monad m => Get a -> (Int -> m (Maybe ByteString)) -> m (Result a)
runGetMaybe g m = go (runGetPartial g)
  where
    go (Partial n ks) = do
        mb <- m n
        case mb of
            Nothing -> return (Left (notEnoughInput [])) -- TODO where is the ctx
            Just b -> case ks b of
                FailureR fl -> return (Left fl)
                SuccessR a -> return (Right a)
                PartialR pt -> go pt

notEnoughInput :: [String] -> Failure
notEnoughInput ctx = Failure mempty ctx "not enough"


label :: String -> Get a -> Get a
label l (Get n p) = Get n (\b ctx ks -> p b (l:ctx) ks)

skip :: Int -> Get ()
skip n = Get n (go n)
  where
    go n0 b0 ctx ks =
        let n1 = n0 - B.length b0
        in case compare n1 0 of
            EQ -> ks mempty ()
            LT -> ks (B.drop n0 b0) ()
            GT -> PartialR (Partial n1 (\b1 -> go n1 b1 ctx ks))

getByte :: Get Word8
getByte = Get 1 go
  where
    go b0 ctx ks = case B.uncons b0 of
        Just (w, b1) -> ks b1 w
        Nothing -> PartialR (Partial 1 (\b1 -> go b1 ctx ks))

getBytes :: Int -> Get Builder
getBytes n = Get n (go mempty n)
  where
    go acc n0 b0 ctx ks =
        let n1 = n0 - B.length b0
        in case compare n1 0 of
            EQ -> ks mempty (byteString b0 <> acc)
            LT -> uncurry ks $ byteString <$> B.splitAt n0 b0
            GT -> PartialR (Partial n1 (\b1 -> go (byteString b0 <> acc) n1 b1 ctx ks))

matchBytes :: ByteString -> Get ()
matchBytes b = Get (B.length b) (go b)
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
                Just br' -> PartialR (Partial n1 (\b1 -> go br' b1 ctx ks))

{-# INLINE getByte #-}
{-# INLINE getBytes #-}
{-# INLINE matchBytes #-}
