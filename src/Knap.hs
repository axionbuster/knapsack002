{-# LANGUAGE MagicHash, UnboxedTuples, ScopedTypeVariables, TypeApplications, FlexibleContexts #-}
module Knap (knap) where
import Control.Monad.ST
import Data.Array.Base
import Data.Int
import GHC.Exts
import GHC.ST

knap
 :: forall w. (Monoid w)
 => (Int -> w) -- ^ on count (number of chosen objects)
 -> (Int -> w) -- ^ on choice (indices)
 -> Int -- ^ max weight
 -> (UArray Int Int16) -- ^ values
 -> (UArray Int Int16) -- ^ weights
 -> w
knap onCount onChoice maxWeight values weights = runST entry where
 entry :: forall s. ST s w
 entry = do
  let count = succ . snd . bounds $ values
  decisions <- newArray @(STUArray s) ((0, 0 :: Int), (count, maxWeight)) False
  counts    <- newArray @(STUArray s) ((0, 0 :: Int), (count, maxWeight)) 0
  let
   workArr = newArray @(STUArray s) (0, maxWeight) (0 :: Int16)
   {-# INLINE workArr #-}
  wO@(STUArray _ _ (I# sz#) wO_) <- workArr
  wI@(STUArray _ _ _        wI_) <- workArr
  let
   knap_ n w | n > count = pure ()
   knap_ n w | w > maxWeight = do
    -- I copy the finished output row to the new input row, instead of swapping
    -- the buffers. As a reward, I don't need to write to the row when
    -- an item is skipped.
    ST $ \s1 -> case safe_scale 2# sz# of
     bytes# -> case copyMutableByteArray# wO_ 0# wI_ 0# bytes# s1 of
      s2 -> (# s2, () #)
    knap_ (n + 1) 1
   knap_ n w | n' <- n - 1 = do
    let bump p = readArray counts (n', p) >>= writeArray counts (n, w) . (+ 1)
    if fromIntegral (weights ! n') > w
    then bump w
    else do
     let w' = w - fromIntegral (weights ! n')
     vtake <- (fromIntegral (values ! n') +) <$> readArray wI w'
     vskip <- readArray wI w
     if vtake > vskip
     then do
      bump w'
      writeArray wO w vtake
      writeArray decisions (n, w) True
     else bump w
  knap_ 1 1
  let
   recon 0 _ = mempty
   recon _ 0 = mempty
   recon n (w :: Int) | n' <- n - 1 = do
    taken <- readArray decisions (n, w)
    if taken
    then recon n' w
    else (onChoice n' <>) <$> recon n' (w - fromIntegral (weights ! n'))
  quantum <- readArray counts (count, maxWeight)
  (onCount quantum <>) <$> recon count maxWeight
