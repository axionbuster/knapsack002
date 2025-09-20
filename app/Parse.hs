{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}
module Parse (M, S, kickoff, option, pair) where
import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.ByteString
import           Data.ByteString.Internal
import           GHC.Exts
import           GHC.ForeignPtr
import           GHC.Int
import           GHC.IO                    hiding (liftIO)
import           GHC.Word
import           Prelude                   hiding (getContents)

-- start, end, strong reference to ByteString's ForeignPtr so it won't finalize
data S = S Addr# Addr# ForeignPtrContents
type M = StateT S (MaybeT IO)

-- rather unusual name, but it's because it does an unusual thing.
kickoff :: M a -> IO a
kickoff m = do
 BS (ForeignPtr a# fpc) (I# len#) <- getContents
 x <- runMaybeT (evalStateT m (S a# (a# `plusAddr#` len#) fpc))
 case x of
  Just y  -> pure y
  Nothing -> fail "Parse: unexpectedly ran out of input somewhere"

option :: (Alternative m) => a -> m a -> m a
option x = (<|> pure x)
{-# INLINE option #-}

pair :: M (Int16, Int16)
pair = (,) <$> (number <* space) <*> (number <* (space <|> eof))
{-# INLINE pair #-}

pop :: M Word8
pop = do
 S s# e# fpc <- get
 case s# `eqAddr#` e# of
  0# -> do
   liftIO
    ( IO $ \s1 -> case readWord8OffAddr# s# 0# s1 of
      (# s2, c# #) -> (# s2, W8# c# #)
    ) <* put (S (s# `plusAddr#` 1#) e# fpc)
  _  -> mzero
{-# INLINE pop #-}

-- skip ' ' or '\n' (actually unconditionally skips 1 byte)
space :: M ()
space = do
 S s# e# fpc <- get
 case s# `eqAddr#` e# of
  0# -> put $ S (s# `plusAddr#` 1#) e# fpc -- no checks
  _  -> mzero
{-# INLINE space #-}

eof :: M ()
eof = do
 S s# e# _ <- get
 case s# `eqAddr#` e# of
  1# -> pure ()
  _  -> mzero
{-# INLINE eof #-}

digit :: M Word8
digit = do
 c <- pop
 guard (0x30 <= c && c <= 0x39) -- '0' .. '9'
 pure $ c - 0x30
{-# INLINE digit #-}

number :: M Int16
number = do
 let
  go !n = option n $ do
   d <- digit
   go (n * 10 + fromIntegral d)
 digit >>= \d -> go (fromIntegral d)
{-# INLINE number #-}
