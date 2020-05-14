{-# LANGUAGE PackageImports   #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.ByteString                            as ByteString
import qualified "base16" Data.ByteString.Base16            as Base16
import qualified "base16-bytestring" Data.ByteString.Base16 as Bos
import           Data.Primitive.ByteArray
import           Data.Primitive.ByteArray.Base16
import qualified Data.Primitive.ByteArray.Base16            as Bytes
import           GHC.Word


input1 :: ByteArray
input1 =
   byteArrayFromList @Word8
     $
--       replicate 16 111
--    <>
      [101, 111, 111]

input2 :: ByteArray
input2 =
   byteArrayFromList @Word8
    $ take 16 $ cycle [101, 111, 111]

ws = [10]
ws' = [48, 97]


input3 :: ByteArray
input3 =
   byteArrayFromList @Word8 $ws'

input4 :: ByteArray
input4 =
   byteArrayFromList @Word8 $ws


main :: IO ()
main =
  do
    print $ ByteString.pack ws
    print $ Bos.encode . ByteString.pack $ ws
    print $ ByteString.unpack . Base16.encodeBase16' . ByteString.pack $ ws
    print $ encode $ input4
    print $ decode . encode $ input4
    print $ decode $ input3
    {-
    putStrLn "input 1"
    putStrLn ""
    putStrLn $ "input: " <> show input1
    putStrLn ""
    let enc = encode input1
    putStrLn $ "encoded: " <> show enc
    putStrLn $ "length enc: " <> (show $ sizeofByteArray enc)
    putStrLn ""
    let dec = decode enc
    putStrLn $ "decoded: " <> show dec
    print $ dec == input1
    putStrLn "input 2"
    putStrLn ""
    putStrLn $ "input: " <> show input2
    putStrLn ""
    let enc = encode input2
    putStrLn $ "encoded: " <> show enc
    putStrLn $ "length enc: " <> (show $ sizeofByteArray enc)
    putStrLn ""
    let dec = decode enc
    putStrLn $ "decoded: " <> show dec
    print $ dec == input2

--}



