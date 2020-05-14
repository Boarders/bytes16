{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

module Data.Primitive.ByteArray.Base16
  ( encode
  , decode
  )
  where

import           Control.Monad.Primitive  (PrimMonad (..), PrimState (..))
import           Control.Monad.ST
import           Data.Primitive.ByteArray (ByteArray (..),
                                           MutableByteArray (..))
import qualified Data.Primitive.ByteArray as ByteArray
import           Data.Primitive.Ptr
import           GHC.Int
import           GHC.Prim
import           GHC.Types


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

decode :: ByteArray -> ByteArray
decode byteArray =
  runST $ do
    mSrc <- ByteArray.unsafeThawByteArray byteArray
    mDest <- decodeM mSrc
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
    let ptrEnd#  = (w816End# `plusAddr#` leftover#)
    go     src# dest# w816End#
    goLeft w816End# (dest# `plusAddr#` jump'#) ptrEnd#
    pure dest
  where
    goLeft :: Addr# -> Addr# -> Addr# -> m ()
    goLeft src# _ ptrEnd# | isTrue# (eqAddr# src# ptrEnd#) = pure ()
    goLeft src# dest# ptrEnd# = do
      primitive $ \s0# ->
        let
          !(# s1# , w# #) = readWord8OffAddr# src# 0# s0#
          !(# w1#, w2# #) = highlow# w#
          !(# off1#, off2# #)
            = (# (timesWord# (w1# `quotWord#` 10##) 39##) `plusWord#` 48##
              ,  (timesWord# (w2# `quotWord#` 10##) 39##) `plusWord#` 48##
              #)

          s2# = writeWord8OffAddr# dest# 0# (w1# `plusWord#` off1#) s1#
          s3# = writeWord8OffAddr# (dest# `plusAddr#` 1#) 0# (w2# `plusWord#` off2#) s2#
        in
          (# s3#, () #)
      goLeft (src# `plusAddr#` 1#) (dest# `plusAddr#` 2#) ptrEnd#


 -- These are repeated but can't be defined at the top level.
 -- The other option is to use CPP
    broadcast16# = broadcastWord8X16# 16##
    broadcast48# = broadcastWord8X16# 48##
    broadcast10# = broadcastWord8X16# 10##
    broadcast39#  = broadcastWord8X16# 39##

    go :: Addr# -> Addr# -> Addr# ->  m ()
    go src# _ end# | isTrue# (eqAddr# src# end#) = pure ()
    go src# dest# end#  = do
        primitive $ \s0# ->
          let
            !(# s1#, word8x16# #) = readWord8X16OffAddr# src# 0# s0#
            !q16# = quotWord8X16# word8x16# broadcast16#
            !r16# = remWord8X16#  word8x16# broadcast16#
            unpackedWords1# = unpackWord8X16# q16#
            unpackedWords2# = unpackWord8X16# r16#
            !(# unpacked1# , unpacked2# #) = shuffle# (# unpackedWords1# , unpackedWords2# #)
            !a8x16#  = packWord8X16# unpacked1#
            !a8x16'#
              = plusWord8X16#
                  (plusWord8X16# a8x16# broadcast48#)
                  (timesWord8X16#
                   (quotWord8X16# a8x16# broadcast10#)
                   broadcast39#)
            !b8x16#  = packWord8X16# unpacked2#
            !b8x16'#
              = plusWord8X16#
                  (plusWord8X16# b8x16# broadcast48#)
                  (timesWord8X16#
                   (quotWord8X16# b8x16# broadcast10#)
                   broadcast39#)
            s2# = writeWord8X16OffAddr# dest# 0# a8x16'# s1#
            s3# = writeWord8X16OffAddr# (dest# `plusAddr#` 16#) 0# b8x16'# s2#
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
        , (#  q8 , r8 , q9  , r9 , q10, r10, q11 , r11
          ,   q12, r12, q13 , r13, q14, r14, q15 , r15
          #)
        #)

decodeM
  :: forall s m . (PrimMonad m, PrimState m ~ s)
  => MutableByteArray s -> m (MutableByteArray s)
decodeM src =
  do
    len <- ByteArray.getSizeofMutableByteArray src
    let !lenHalf = len `div` 2
    dest <- ByteArray.newPinnedByteArray lenHalf
    let !(Ptr dest#) = ByteArray.mutableByteArrayContents dest
    let !(Ptr src# ) = ByteArray.mutableByteArrayContents src
    let !(I# word8x32Len#, I# leftover#) = len `quotRem` 32
    let !jump#  = word8x32Len# *# 16#
    let !jump'# = word8x32Len# *# 32#
    let w8x32End# = (src# `plusAddr#` jump'#)
    let ptrEnd#  = (w8x32End# `plusAddr#` leftover#)-- `plusAddr#` 1#)
    go src# dest# w8x32End#
    goLeft w8x32End# (dest# `plusAddr#` jump#) ptrEnd#
    pure dest
  where
    goLeft :: Addr# -> Addr# -> Addr# -> m ()
    goLeft src# _ ptrEnd# | isTrue# (eqAddr# src# ptrEnd#) = pure ()
    goLeft src# dest# ptrEnd# = do
      primitive $ \s0# ->
        let
          !(# s1# , q'# #) = readWord8OffAddr# src# 0# s0#
          !(# s2# , r'# #) = readWord8OffAddr# (src# `plusAddr#` 1#) 0# s1#
          !q# = (q'# `minusWord#` 48##) `minusWord#` (timesWord# (q'# `quotWord#` 64##) 39##)
--          !a' = unsafePerformIO $ print (W# q#)
          !r# = (r'# `minusWord#` 48##) `minusWord#` (timesWord# (r'# `quotWord#` 64##) 39##)
--          !b' = unsafePerformIO $ print (W# r#)
          !w256# = (q# `timesWord#` 16##) `plusWord#` r#
          s3# = writeWord8OffAddr# dest# 0# w256# s2#
        in
          (# s3#, () #)
      goLeft (src# `plusAddr#` 2#) (dest# `plusAddr#` 1#) ptrEnd#


    broadcast16# = broadcastWord8X16# 16##
    broadcast48# = broadcastWord8X16# 48##
    broadcast39# = broadcastWord8X16# 39##
    broadcast64# = broadcastWord8X16# 64##

    go :: Addr# -> Addr# -> Addr# ->  m ()
    go src# _ end# | isTrue# (eqAddr# src# end#) = pure ()
    go src# dest# end#  = do
        primitive $ \s0# ->
          let
            !(# s1#, a8x16x# #) = readWord8X16OffAddr# src# 0# s0#
            !(# s2#, b8x16x# #) = readWord8X16OffAddr# (src# `plusAddr#` 16#) 0# s1#
            !a8x16x'#
              = minusWord8X16#
                 (minusWord8X16# a8x16x# broadcast48#)
                 (timesWord8X16#
                   (quotWord8X16# a8x16x# broadcast64#)
                   broadcast39#)
            !b8x16x'#
              = minusWord8X16#
                 (minusWord8X16# b8x16x# broadcast48#)
                 (timesWord8X16#
                   (quotWord8X16# b8x16x# broadcast64#)
                   broadcast39#)

            unpackedWords1# = unpackWord8X16# a8x16x'#
            unpackedWords2# = unpackWord8X16# b8x16x'#
            !(# unpackedQ8# , unpackedR8# #) = deshuffle# (# unpackedWords1# , unpackedWords2# #)
            !q8# = packWord8X16# unpackedQ8#
            !r8# = packWord8X16# unpackedR8#
            w256'8X16# = plusWord8X16# (timesWord8X16# q8# broadcast16#) r8#
            s3# = writeWord8X16OffAddr# dest# 0# w256'8X16# s2#
          in
            (# s3#, () #)
        go (src# `plusAddr#` 32#) (dest# `plusAddr#` 16#) end#

    deshuffle#
      :: (# UnpackedWord8X16# , UnpackedWord8X16# #)
      -> (# UnpackedWord8X16# , UnpackedWord8X16# #)

    deshuffle#
      (#
        (# q0 , r0 , q1 , r1 , q2 , r2 , q3 , r3
        ,  q4 , r4 , q5 , r5 , q6 , r6 , q7 , r7
        #)
      , (# q8 , r8 , q9 , r9, q10 , r10 , q11 , r11
        ,  q12 , r12, q13, r13, q14, r14, q15, r15
        #)
      #) =
      (#
        (# q0 , q1 , q2 , q3 , q4 , q5 , q6 , q7
        ,  q8 , q9, q10, q11 , q12, q13, q14, q15
        #)
      , (# r0 , r1 , r2 , r3, r4 , r5 , r6 , r7
        ,  r8 , r9, r10, r11, r12, r13, r14, r15
        #)
      #)
