{-# LANGUAGE OverloadedStrings #-}

module QC.ByteString (tests) where

import QC.Common
import Data.Wire.Get
import Data.Wire.Get.ByteString

import Test.QuickCheck hiding (label)
import Test.Tasty.QuickCheck (testProperty)
import Data.Bifunctor
import Data.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

getBytesPositive =
    forAllShrink nonEmptyInputs shrinkNonEmptyInput $ \li  ->
    forAllShrink chunks         shrinkChunk         $ \lc  ->
    forAllShrink chunks         shrinkChunk         $ \mlc ->
    forAllShrink nonEmptyInputs shrinkNonEmptyInput $ \mi  ->
    forAllShrink chunks         shrinkChunk         $ \mrc ->
    forAllShrink chunks         shrinkChunk         $ \rc  ->
    forAllShrink nonEmptyInputs shrinkNonEmptyInput $ \ri  ->
    let l = li <> [lc]
        m = [mlc] <> mi <> [mrc]
        r = [rc] <> ri
        i = li <> [lc <> mlc] <> mi <> [mrc <> rc] <> ri
        ls = L.fromChunks l
        ms = L.fromChunks m
        rs = L.fromChunks r
        get = (\l' m' r' -> ls === l' .&&. rs === r' .&&. ms === m')
          <$> (label "l" . getBytesLazy $ inputLength l)
          <*> (label "m" . getBytesLazy $ inputLength m)
          <*> (label "r" . getBytesLazy $ inputLength r)
        (res, rest) = runGetChunks get i
    in  counterexample ("rest: " <> show rest) $
        counterexample ("result: " <> show (() <$ res)) $
        case res of
            Right prop -> prop
            Left fl -> property False

skipPositive =
    forAllShrink nonEmptyInputs shrinkNonEmptyInput $ \li  ->
    forAllShrink chunks         shrinkChunk         $ \lc  ->
    forAllShrink chunks         shrinkChunk         $ \mlc ->
    forAllShrink nonEmptyInputs shrinkNonEmptyInput $ \mi  ->
    forAllShrink chunks         shrinkChunk         $ \mrc ->
    forAllShrink chunks         shrinkChunk         $ \rc  ->
    forAllShrink nonEmptyInputs shrinkNonEmptyInput $ \ri  ->
    let l = li <> [lc]
        m = [mlc] <> mi <> [mrc]
        r = [rc] <> ri
        i = li <> [lc <> mlc] <> mi <> [mrc <> rc] <> ri
        ls = L.fromChunks l
        ms = L.fromChunks m
        rs = L.fromChunks r
        get = skip (inputLength l) *> skip (inputLength m) *> skip (inputLength r)
        res = runGetChunks get i
    in  res === (Right (), [])

tests =
    [ testProperty "getBytesPositive" getBytesPositive
    , testProperty "skipPositive" skipPositive
    ]
