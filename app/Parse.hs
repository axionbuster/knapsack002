{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}
module Parse (M, S, kickoff, option, pair) where
import           Control.Applicative
import           Control.Monad
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

-- surely IO has a MonadPlus instance for exception handling, which is useful
-- for signaling parsing failure (but not *error*). failure is an acceptable
-- outcome (e.g., terminating input: obviously, input must terminate somewhere).
--
-- but the issue is twofold. first, it's incorrect because it doesn't backtrack
-- on the parser state, and second it's inefficient because it actually involves
-- the native exception mechanism. this is not Python here.
type M = StateT S (MaybeT IO)

-- a rather unusual name, but it's because it does an unusual thing.
-- we read the entire input and start parsing it. as we parse, we plug data into
-- the monadic action m. this way we get blazingly fast inversion of control.
kickoff :: M a -> IO a
kickoff m = do
 -- at first we used a naive bytestring parser using nextInt
 -- (Data.ByteString.Char8), which was really convenient (and required only
 -- about 1/12 of the lines of code as this module). but unfortunately it kept
 -- building up and tearing down little closures for the Maybe and ByteString
 -- (BS) constructors, and it stressed the garbage collector enough to add a
 -- meaningful number of milliseconds of pauses. so here we opt for a more tight
 -- control over the allocation. this is so overkill in general, though, because
 -- GHC Haskell's GC is still the state of the art.
 BS (ForeignPtr a# fpc) (I# len#) <- getContents
 x <- runMaybeT (evalStateT m (S a# (a# `plusAddr#` len#) fpc))
 case x of
  Just y  -> pure y
  Nothing -> fail "Parse: unexpectedly ran out of input somewhere"

option :: (Alternative m) => a -> m a -> m a
option x = (<|> pure x)
{-# INLINE option #-}

-- isn't it lovely how this code compiles down to very efficient code?
pair :: M (Int16, Int16)
pair = (,) <$> (number <* space) <*> (number <* (space <|> eof))
{-# INLINE pair #-}

-- get a single byte, or else fail. when it fails, it's eof (if input is valid).
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
