module Main (main) where

import qualified QC.Buffer as Buffer
import qualified QC.ByteString as ByteString
import Test.Tasty (defaultMain, testGroup)

main = defaultMain tests

tests = testGroup "tests"
    [ testGroup "buffer" Buffer.tests
    , testGroup "bytestring" ByteString.tests
    ]
