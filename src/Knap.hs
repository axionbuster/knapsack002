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
  taken <- newArray @(STUArray s) ((0, 0), (count, maxWeight)) False
  -- w1 and w2 contain sums of values, and, so, they can grow really big.
  w1 <- newArray @(STUArray s) (0, maxWeight) (0 :: Int32)
  w2 <- newArray @(STUArray s) (0, maxWeight) (0 :: Int32)
  let
   knap_ n _ _  _  | n > count = pure ()
   knap_ n w wO wI | w > maxWeight = knap_ (n + 1) 1 wI wO
   knap_ n w wO wI | n' <- n - 1 = do
    if w - (weights ! n') >= 0 -- values are too small to overflow
    then do
     let !w' = w - (weights ! n')
     vtake <- (fromIntegral (values ! n') +) <$> readArray wI w'
     vskip <- readArray wI w
     if vtake > vskip
     then do
      writeArray taken (n, w) True
      writeArray wO w vtake
     else readArray wI w >>= writeArray wO w
    else  readArray wI w >>= writeArray wO w
    knap_ n (w + 1) wO wI
  knap_ 1 1 w1 w2
  let
   release c = pure . Thought (onCount c)
   recon 0 _ !c x = release c x
   recon _ 0 !c x = release c x
   recon n w !c x | n' <- n - 1 = do
    -- [order]:
    --  onChoice n' <> x -- good
    --  x <> onChoice n' -- bad! alloc-fest!
    t <- readArray taken (n, w)
    if t
    then recon n' (w - (weights ! n')) (c + 1) (onChoice n' <> x) -- [order]
    else recon n' w c x
  recon count maxWeight 0 mempty
