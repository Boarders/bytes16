{-# LANGUAGE TypeApplications #-}
module Main where

import           Data.Primitive.ByteArray
import           Data.Primitive.ByteArray.Base16
import           GHC.Word

input1 :: ByteArray
input1 =
   byteArrayFromList @Word8
     $ replicate 16 111
    <> [101, 111, 111]

input2 :: ByteArray
input2 =
   byteArrayFromList @Word8
    $ [101, 111, 111]

main :: IO ()
main =
  do
    putStrLn "input 1:"
    putStrLn ""
    print input1
    putStrLn ""
    print $ encode input1
    putStrLn ""
    putStrLn ""
    putStrLn "input 2"
    putStrLn ""
    print input2
    putStrLn ""
    print $ encode input2





