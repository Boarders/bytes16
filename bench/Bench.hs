{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Criterion.Main                             as C (bench, bgroup,
                                                                  defaultMain,
                                                                  env, nf, whnf)
import qualified Data.ByteString                            as ByteString
import qualified "base16" Data.ByteString.Base16            as Base16
import qualified "base16-bytestring" Data.ByteString.Base16 as Bos
import qualified Data.Primitive.ByteArray.Base16            as Bytes

import           Control.DeepSeq
import qualified Data.Primitive.ByteArray                   as ByteArray
import           Data.Word

input :: Int -> [Word8]
input n = take n $ cycle [0..255]

instance NFData ByteArray.ByteArray where
  rnf = rwhnf

getInputs :: IO (ByteArray.ByteArray, ByteString.ByteString)
getInputs =
  do
    let inp = input 10000
    pure ( ByteArray.byteArrayFromList inp
         , ByteString.pack inp
         )

main :: IO ()
main = C.defaultMain . pure $
  C.env getInputs $ \ ~(bytes, bs) ->
      C.bgroup "base16 benchmarks:"
        [ C.bench "bytes16-encode"
            $ C.whnf Bytes.encode bytes
        , C.bench "base16-bytestring-encode"
            $ C.whnf Bos.encode bs
        , C.bench "base16-encode"
            $ C.whnf Base16.encodeBase16' bs
        , C.bench "bytes16-decode-encode"
            $ C.whnf (Bytes.decode . Bytes.encode) bytes
        , C.bench "base16-bytestring-decode-encode"
            $ C.whnf (fst . Bos.decode . Bos.encode) bs
        , C.bench "base16-decode-encode"
            $ C.whnf (Base16.decodeBase16Lenient . Base16.encodeBase16') bs
        ]

