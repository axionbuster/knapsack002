{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnboxedTuples       #-}
module Knap (knap, Thought, think) where
import           Control.Monad.ST
import           Data.Array.Base
import           Data.Int
import           GHC.Exts
import           GHC.ST

-- strict constructor means work is done when it's forced to WHNF.
-- this is important because the `par` parallel combinator works by
-- making this forcing process parallel.
data Thought w = Thought !w !w -- on count, on choice

think :: (Semigroup w) => Thought w -> w
think (Thought a b) = a <> b

-- | 0-1 Knapsack solver (dynamic programming).
--
-- Event handling:
-- - `onCount`: header.
-- - `onChoice`: for each index number chosen, order arbitrary.
knap
 :: forall w. (Monoid w)
 => (Int16 -> w) -- ^ on count (number of chosen objects)
 -> (Int16 -> w) -- ^ on choice (indices)
 -> Int16 -- ^ max weight
 -> UArray Int16 Int16 -- ^ values
 -> UArray Int16 Int16 -- ^ weights
 -> Thought w
knap onCount onChoice maxWeight values weights = runST entry where
 entry :: forall s. ST s (Thought w)
 entry = do
  let !count = succ . snd . bounds $ values
  -- an item at (n, w) is taken if and only if readArray counts (n, w)
  -- returns a negative number. note that every item has a positive value.
  counts <- newArray @(STUArray s) ((0, 0), (count, maxWeight)) 0
  -- wO and wI contain sums of values, and, so, they can grow really big.
  wO <- newArray @(STUArray s) (0, maxWeight) (0 :: Int32)
  wI <- newArray @(STUArray s) (0, maxWeight) (0 :: Int32)
  let
   knap_ n _ | n > count = pure ()
   knap_ n w | w > maxWeight = do
    let !(STUArray _ _ (I# sz#) wI_) = wI
        !(STUArray _ _ _        wO_) = wO
    -- copy the working memory so that when i skip an item the decision is
    -- just copied. but it's such bullshit the standard library lacks memcpy
    -- so i have to reach out to a primitive.
    ST $ \s1 ->
     case copyMutableByteArray# wO_ 0# wI_ 0# (safe_scale 4# sz#) s1 of
      s2 -> (# s2, () #)
    knap_ (n + 1) 1
   knap_ n w | n' <- n - 1 = do
    if w - (weights ! n') >= 0 -- values are too small to overflow
    then do
     let !w' = w - (weights ! n')
     vtake <- (fromIntegral (values ! n') +) <$> readArray wI w'
     vskip <- readArray wI w
     if vtake > vskip
     then do
      readArray counts (n', w') >>= writeArray counts (n, w).negate.(+ 1).abs
      writeArray wO w vtake
     else readArray counts (n', w) >>= writeArray counts (n, w) . abs
    else  readArray counts (n', w) >>= writeArray counts (n, w) . abs
    knap_ n (w + 1)
  knap_ 1 1
  let
   recon 0 _ = mempty
   recon _ 0 = mempty
   recon n w | n' <- n - 1 = do
    taken <- readArray counts (n, w)
    if taken < 0
    then (onChoice n' <>) <$> recon n' (w - (weights ! n'))
    else recon n' w
  quantum <- abs <$> readArray counts (count, maxWeight)
  Thought (onCount quantum) <$> recon count maxWeight
