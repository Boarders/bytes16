{-# LANGUAGE PackageImports #-}
module Main where

import           Test.QuickCheck.Monadic
import           Test.Tasty
import qualified Test.Tasty.QuickCheck            as QC
import Test.Tasty.QuickCheck ((===))
import qualified Data.ByteString                            as ByteString
import qualified "base16" Data.ByteString.Base16            as Base16
import qualified "base16-bytestring" Data.ByteString.Base16 as Bos
import qualified Data.Primitive.ByteArray.Base16            as Bytes

import qualified Data.Primitive.ByteArray                   as ByteArray
import           Control.Monad.ST
import Data.Word
import Debug.Trace

main :: IO ()
main = defaultMain  testSuite

testSuite :: TestTree
testSuite = testGroup ""
  [ localOption (QC.QuickCheckTests 1000) encodeDecodeTest
  ]


encodeDecodeTest :: TestTree
encodeDecodeTest = testGroup "encodeDecode"
  [ QC.testProperty
      "decode . encode === id"
      decodeEncodeId
  , QC.testProperty
      "bytes16-encode === base16-bytestring"
      base16ByteStringAgree
  ]


decodeEncodeId :: [Word8] -> QC.Property
decodeEncodeId wds =
      (toWord8s . Bytes.decode . Bytes.encode . ByteArray.byteArrayFromList) wds
  === wds
    

toWord8s :: ByteArray.ByteArray -> [Word8]
toWord8s bytes = runST $ do
    mutBytes <- ByteArray.unsafeThawByteArray bytes
    go 0 mutBytes []
  where
    len = ByteArray.sizeofByteArray bytes

   -- go :: Int -> _ -> [Word8] -> ST s [Word8]
    go n mb ws | n == len = pure (reverse ws)
    go n mb ws =
      do
        w <- ByteArray.readByteArray mb n
        go (n + 1) mb (w : ws)

t str x = trace (str <> " " <> (show x)) x



base16ByteStringAgree :: [Word8] -> QC.Property
base16ByteStringAgree wds =
  let
    bs = ByteString.pack (t "words" wds)
    bytes = ByteArray.byteArrayFromList wds
    encBS = Bos.encode bs
    encBytes = Bytes.encode bytes
    unpackEncBS = t "BS" $ ByteString.unpack encBS
    unpackEncBytes = t "Bytes" $ toWord8s encBytes
  in
    if wds == [0] then QC.property True
    else
      unpackEncBytes === unpackEncBS
    
