{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns  #-}
module Knap (Thought, think, knap) where

import           Control.Monad
import           Data.Array.Base
import           Data.Bits
import           Data.Int
import           GHC.Exts
import           GHC.ST

data Thought a = Thought !a !a

think :: (Semigroup a) => Thought a -> a
think (Thought a b) = a <> b

knap
 :: (Monoid w)
 => (Int16 -> w)
 -> (Int16 -> w)
 -> Int16
 -> UArray Int16 Int16
 -> UArray Int16 Int16
 -> Thought w
knap onCount onChoice (unI16 -> maxWeight) values weights = runST entry where
 entry = do
  let
   realNWords = ceilq (maxWeight + 1)
   count = unI16 . succ . snd . bounds $ values
   newM (I# sz#) = ST $ \s0 -> case newByteArray# sz# s0 of
    (# s1, mba# #) -> case setByteArray# mba# 0# sz# 0# s1 of
     s2 -> (# s2, M mba# #)
   {-# INLINE newM #-}
  M taken# <- newM (wordScale ((count + 1) * realNWords))
  M v1#    <- newM ((maxWeight + 1) .<<. 2) -- Int32
  M v2#    <- newM ((maxWeight + 1) .<<. 2) -- Int32
  let
   int16 = fromIntegral
   go n = when (n <= count) $ do
    let
     !(# vo#, vi# #) | odd n = (# v1#, v2# #) | otherwise = (# v2#, v1# #)
     vn = unI16 $ values  ! int16 (n - 1)
     wn = unI16 $ weights ! int16 (n - 1)
    knapRow taken# vo# vi# n vn wn maxWeight
    go (n + 1)
   release c = pure . Thought (onCount (int16 c))
   recon 0 _ !c m = release c m
   recon _ 0 !c m = release c m
   recon n w !c m | n' <- n - 1 = do
    t <- ST $ \s0 -> case n * realNWords * wordBits + w of
     I# wx# -> case readWordArray# taken# (bOOL_INDEX wx#) s0 of
      (# s1, word# #) -> case word# `and#` bOOL_BIT wx# of
       bit# -> (# s1, isTrue# (bit# `neWord#` 0##) #)
    if t
    then do
     recon
      n'
      (w - unI16 (weights ! int16 n'))
      (c + 1)
      (onChoice (int16 n') <> m)
    else recon n' w c m
  go 1
  recon count maxWeight 0 mempty

unI16 :: Int16 -> Int
unI16 = fromIntegral
{-# INLINE unI16 #-}

wordScale :: Int -> Int
wordScale (I# x#) = I# (wORD_SCALE x#)

data M s = M (M# s)
type M#  = MutableByteArray#

wordBits :: Int
wordBits = finiteBitSize (0 :: Word)

ceilq, floorq :: Int -> Int
ceilq  i = (i + wordBits - 1) `quot` wordBits
floorq i = i `quot` wordBits

knapRow :: M# s -> M# s -> M# s -> Int -> Int -> Int -> Int -> ST s ()
knapRow taken# vo# vi# n !vn !wn wmax = do
 let realNWords = ceilq (wmax + 1)
 -- example situation: each dot represents a byte. 4 words (64-bit) shown.
 -- ........|........|........|........| w  ([0,255])
 --            *                       | wn (example)
 -- xxxxxxxx|xx                        | unconditionally skip
 --             ?????|????????|????????| decide
 --
 -- this step pertains to the "unconditionally skip" part.
 -- we allow overlapping with the first word where decisions are made.
 ST $ \s0 ->
  case wmax + 1 `min` wn of
   I# x -> case safe_scale 4# x of
    -- copyMutableByteArray#: source -> dest
    fill# -> case copyMutableByteArray# vi# 0# vo# 0# fill# s0 of
     s1 -> (# s1, () #)
 let
  writeW (I# i#) (W# p#) = ST $ \s0 ->
   case writeWordArray# taken# i# p# s0 of
    s1 -> (# s1, () #)
  write32 (I# i#) (I# v#) = ST $ \s0 ->
   case writeInt32Array# vo# i# (intToInt32# v#) s0 of
    s1 -> (# s1, () #)
  {-# INLINE write32 #-}
  read32 (I# i#) = ST $ \s0 -> case readInt32Array# vi# i# s0 of
   (# s1, i32# #) -> (# s1, I# (int32ToInt# i32#) #)
  {-# INLINE read32 #-}
  indexW w = realNWords * n + floorq w
  boolToW = fromIntegral . fromEnum
  go w k p | w <= wmax = do
   vtake <- (vn +) <$> read32 (w - wn)
   vskip <-            read32  w
   let
    -- record this bit, flush if needed, and then move on.
    next x
     | k == wordBits = writeW (indexW (w - 1)) p >> go (w + 1) 1 (boolToW x)
     | x             = go (w + 1) (k + 1) (setBit p k)
     | otherwise     = go (w + 1) (k + 1) p
    {-# INLINE next #-}
   if vtake > vskip
   then write32 w vtake >> next True
   else write32 w vskip >> next False
  go w _ p = writeW (indexW (w - 1)) p
 let !gap = wn - wordBits * floorq wn
 go wn gap 0
