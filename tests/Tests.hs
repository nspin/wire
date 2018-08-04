-- module Main (main) where

{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Builder
import Data.Wire.Get
import Data.Wire.Get.Internal
import Control.Monad
import Data.Bifunctor

main :: IO ()
main = void $ do
    print $ runGetStrict getByte "foobar"
    print $ first (fmap toLazyByteString) $ runGetStrict (getBytes 10) "ababfdsaljfdksal;gjkl;sagjkl"
