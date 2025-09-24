{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples    #-}
{-# LANGUAGE ViewPatterns     #-}
module Knap (main_) where
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Array.Base
import           Data.Array.IO
import           Data.Bits
import qualified Data.ByteString.Builder as B
import           Data.Int
import           Data.List               (intersperse)
import           GHC.Conc
import           GHC.Exts
import           GHC.ST
import           Parse
import           System.IO

main_ :: IO ()
main_ = do
 let
  work = do
   (cap, num) <- pair
   dvs <- liftIO $ newArray_ @IOUArray (0, num - 1)
   dws <- liftIO $ newArray_ @IOUArray (0, num - 1)
   let
    rep i | i == num = do
     vs <- liftIO $ unsafeFreeze @_ @_ @_ @_ @UArray dvs
     ws <- liftIO $ unsafeFreeze @_ @_ @_ @_ @UArray dws
     let
      -- 0x0a = line feed; 0x20 = space
      k = knap
       (\n -> B.int16Dec n <> B.word8 0x0a) -- announce count
       (\j -> B.int16Dec j <> B.word8 0x20) -- announce index
       cap vs ws
     k `par` pure k
    rep i = do
     (v, w) <- pair
     liftIO $ writeArray dvs i v
     liftIO $ writeArray dws i w
     rep $ i + 1
   rep 0
 kickoff (some work) >>=
  B.hPutBuilder stdout .
  mconcat .
  intersperse (B.word8 0x0a) .
  map think

-- | 'GHC.Conc.par' is used for deterministic parallelism.
-- @a `par` b@ works by beginning the reduction of @a@ in a spark that will
-- be evaluated in parallel as @b@ is evaluated. otherwise, it's the same
-- as @b@. in particular, @_|_ `bar` b@ is equal to @b@ (so it's not `seq`).
--
-- we are using 'GHC.Conc.par' to make the evalaluation of 'knap' work
-- in parallel. and by making this require a strict constructor, we know that
-- when a \'thought\' is foced, its two fields will also be forced, and that's
-- when 'knap' will actually begin working.
data Thought a = Thought !a !a

think :: (Semigroup a) => Thought a -> a
think (Thought a b) = a <> b

knap
 :: (Monoid w)
 => (Int16 -> w) -- ^ handle count announcement
 -> (Int16 -> w) -- ^ handle choice (index) announcement
 -> Int16 -- ^ max weight
 -> UArray Int16 Int16 -- ^ values (0-indexed)
 -> UArray Int16 Int16 -- ^ weights (0-indexed)
 -> Thought w
knap onCount onChoice (unI16 -> maxWeight) values !weights = runST entry where
 entry = do
  let
   realNWords = ceilq (maxWeight + 1)
   count = unI16 . succ . snd . bounds $ values
   newM (I# sz#) = ST $ \s0 -> case newByteArray# sz# s0 of
    (# s1, mba# #) -> case setByteArray# mba# 0# sz# 0# s1 of
     s2 -> (# s2, M mba# #)
   {-# INLINE newM #-}
   int16 = fromIntegral
   {-# INLINE int16 #-}
   release c = pure . Thought (onCount (int16 c))
  -- we use a matrix of decisions (bools) packed into a bitset. note that this
  -- bitset is aligned to the word size boundary. this is important for a number
  -- of unsafe operations in knapRow. for the actual optimal values, we store
  -- them in two running rows and swap them every time. now note that
  -- w_max <= 10_000, n <= 3_000. the product w_max * n does not fit in an Int16
  -- so we use an Int32 array.
  M taken# <- newM (wordScale ((count + 1) * realNWords))
  M v1#    <- newM ((maxWeight + 1) .<<. 2) -- Int32
  M v2#    <- newM ((maxWeight + 1) .<<. 2) -- Int32
  let
   go n = when (n <= count) $ do
    let
     !(# vo#, vi# #) | odd n = (# v1#, v2# #) | otherwise = (# v2#, v1# #)
     vn = unI16 $ values  ! int16 (n - 1)
     wn = unI16 $ weights ! int16 (n - 1)
    knapRow taken# vo# vi# n vn wn maxWeight
    go (n + 1)
  let
   recon 0 !_ !c m = release c m
   recon _ !0 !c m = release c m
   recon n !w !c m | n' <- n - 1 = do
    -- read a single bit from the bitset.
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
  -- so it's really easy. first we find the decision matrix, then we walk it
  -- backwards, reconstructing our choices.
  go 1
  recon count maxWeight 0 mempty
{-# INLINE knap #-}

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

-- a "row" means we fix the 'n' subproblem parameter.
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
 -- see the decision bits stay as 0 (don't take). we 0-initialize that bitset
 -- so we do nothing on it. and as to the optimum values for each subproblem
 -- (by varying weights), we just copy the optimum values from the previous
 -- row (n - 1). note: it doesn't hurt to copy too much, but copying too little
 -- is incorrect.
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
 let
  -- it's really important that the bitset is aligned to the word boundary.
  -- or else, memory access here can cause an "unchecked exception" (~
  -- undefined behavior).
  go w !k p | w <= wmax = do
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
 -- we take a little bit of shortcut by starting in the middle of the word.
 go wn (wn - wordBits * floorq wn) 0
