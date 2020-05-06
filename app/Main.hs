{-# LANGUAGE TypeApplications #-}
module Main where

import           Data.Primitive.ByteArray
import           Data.Primitive.ByteArray.Base16
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
    

main :: IO ()
main =
  do
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





