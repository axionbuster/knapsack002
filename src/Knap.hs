{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnboxedTuples       #-}
module Knap (knap) where
import           Control.Monad
import           Control.Monad.ST
import           Data.Array.Base
import           Data.Int
import           GHC.Exts
import           GHC.ST
import           GHC.Ix

knap
 :: forall w. (Monoid w)
 => (Int -> w) -- ^ on count (number of chosen objects)
 -> (Int -> w) -- ^ on choice (indices)
 -> Int -- ^ max weight
 -> UArray Int Int16 -- ^ values
 -> UArray Int Int16 -- ^ weights
 -> w
knap onCount onChoice maxWeight values weights = runST entry where
 entry :: forall s. ST s w
 entry = do
  let count = succ . snd . bounds $ values
  decisions <- newArray @(STUArray s) ((0, 0 :: Int), (count, maxWeight)) False
  counts@(STUArray cl cu _ cs#) <-
   newArray @(STUArray s) ((0, 0 :: Int), (count, maxWeight)) 0
  let
   workArr = newArray @(STUArray s) (0, maxWeight) (0 :: Int16)
   {-# INLINE workArr #-}
  wO@(STUArray _ _ (I# sz#) wO_) <- workArr
  wI@(STUArray _ _ _        wI_) <- workArr
  let
   knap_ n _ | n > count = pure ()
   knap_ n w | w > maxWeight = do
    -- I copy the finished output row to the new input row, instead of swapping
    -- the buffers. As a reward, I don't need to write to the row when
    -- an item is skipped. Same thing for the counts matrix.
    ST $ \s1 -> case safe_scale 2# sz# of
     bytes# -> case copyMutableByteArray# wO_ 0# wI_ 0# bytes# s1 of
      s2 -> case unsafeIndex (cl, cu) (n, w) of
       I# o# -> case maxWeight + 1 of
        I# mwp1# -> case wORD_SCALE mwp1# of
         len# -> case copyMutableByteArray# cs# o# cs# (o# +# len#) len# s2 of
          s3 -> (# s3, () #)
    knap_ (n + 1) 1
   knap_ n w | n' <- n - 1 = do
    unless (fromIntegral (weights ! n') > w) $ do
     let w' = w - fromIntegral (weights ! n')
     vtake <- ((values ! n') +) <$> readArray wI w'
     vskip <- readArray wI w
     when (vtake > vskip) $ do
      readArray counts (n', w') >>= writeArray counts (n, w) . (+ 1)
      writeArray wO w vtake
      writeArray decisions (n, w) True
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
