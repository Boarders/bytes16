{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

module Data.Primitive.ByteArray.Base16 where

import           Control.Monad.Primitive  (PrimMonad (..), PrimState (..))
import           Control.Monad.ST
import           Data.Primitive.ByteArray (ByteArray (..),
                                           MutableByteArray (..))
import qualified Data.Primitive.ByteArray as ByteArray
import           Data.Primitive.Ptr
import           GHC.Int
import           GHC.Prim



type UnpackedWord8X16# =
  (# Word# , Word# , Word# , Word#
   , Word# , Word# , Word# , Word#
   , Word# , Word# , Word# , Word#
   , Word# , Word# , Word# , Word#
   #)


encode :: ByteArray -> ByteArray
encode byteArray =
  runST $ do
    mSrc <- ByteArray.unsafeThawByteArray byteArray
    mDest <- encodeM mSrc
    ByteArray.unsafeFreezeByteArray mDest

encodeM
  :: forall s m . (PrimMonad m, PrimState m ~ s)
  => MutableByteArray s -> m (MutableByteArray s)
encodeM src =
  do
    len <- ByteArray.getSizeofMutableByteArray src
    dest <- ByteArray.newPinnedByteArray (len * 2)
    let !(Ptr dest#) = ByteArray.mutableByteArrayContents dest
    let !(Ptr src# ) = ByteArray.mutableByteArrayContents src
    let !(I# word8x16Len#, I# leftover#) = len `quotRem` 16
    let !jump#  = word8x16Len# *# 16#
    let !jump'# = word8x16Len# *# 32#
    let w816End# = (src# `plusAddr#` jump#)
    let ptrEnd#  = (w816End# `plusAddr#` leftover# `plusAddr#` 1#)
    go     src# dest# w816End#
    goLeft w816End# (dest# `plusAddr#` jump'#) ptrEnd#
    pure dest
  where
    goLeft :: Addr# -> Addr# -> Addr# -> m ()
    goLeft src# _ ptrEnd# | I# (eqAddr# src# ptrEnd#) == 1 = pure ()
    goLeft src# dest# ptrEnd# = do
      primitive \s0# ->
        let
          !(# s1# , w# #) = readWord8OffAddr# src# 0# s0#
          !(# w1#, w2# #) = highlow# w#
          s2# = writeWord8OffAddr# dest# 0# w1# s1#
          s3# = writeWord8OffAddr# (dest# `plusAddr#` 1#) 0# w2# s2#
        in
          (# s3#, () #)
      goLeft (src# `plusAddr#` 1#) (dest# `plusAddr#` 2#) ptrEnd#


    broadcast16# = broadcastWord8X16# 16##

    go :: Addr# -> Addr# -> Addr# ->  m ()
    go src# _ end# | (I# (eqAddr# src# end#)) == 1 = pure ()
    go src# dest# end#  = do
        primitive \s0# ->
          let
            !(# s1#, word8x16# #) = readWord8X16OffAddr# src# 0# s0#
            !q16# = quotWord8X16# word8x16# broadcast16#
            !r16# = remWord8X16#  word8x16# broadcast16#
            unpackedWords1# = unpackWord8X16# q16#
            unpackedWords2# = unpackWord8X16# r16#
            !(# unpacked1# , unpacked2# #) = shuffle# (# unpackedWords1# , unpackedWords2# #)
            a8x16x# = packWord8X16# unpacked1#
            b8x16x# = packWord8X16# unpacked2#
            s2# = writeWord8X16OffAddr# dest# 0# a8x16x# s1#
            s3# = writeWord8X16OffAddr# (dest# `plusAddr#` 16#) 0# b8x16x# s2#
          in
            (# s3#, () #)
        go (src# `plusAddr#` 16#) (dest# `plusAddr#` 32#) end#

    highlow# :: Word# -> (# Word#, Word# #)
    highlow# w# = w# `quotRemWord#` 16##

    shuffle#
      :: (# UnpackedWord8X16# , UnpackedWord8X16# #)
      -> (# UnpackedWord8X16# , UnpackedWord8X16# #)

    shuffle#
      (#
        (# q0 , q1 , q2 , q3 , q4 , q5 , q6 , q7
        ,  q8 , q9, q10, q11 , q12, q13, q14, q15
        #)
      , (# r0 , r1 , r2 , r3, r4 , r5 , r6 , r7
        ,  r8 , r9, r10, r11, r12, r13, r14, r15
        #)
      #) =
        (#
          (# q0 , r0 , q1  , r1, q2 , r2 , q3  , r3
          ,  q4 , r4 , q5  , r5, q6 , r6 , q7  , r7
          #)
        , (# q8 , r8 , q9  , r9 , q10, r10, q11 , r11
          ,  q12, r12, q13 , r13, q14, r14, q15 , r15
          #)
        #)
