{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}
module Growable (Growable, newGr, pushGr, freezeGr, resetGr) where
import           Control.Monad
import           Data.Array.Base
import           Data.Array.IO.Internals
import           Data.Int
import           Data.IORef
import           GHC.Exts
import           GHC.IO

type IA = IOUArray Int Int
data Growable = Growable IA (IORef (IOUArray Int Int16))

newIA :: Int -> IO IA
newIA = newArray (0, 0)

writeIA :: IA -> Int -> IO ()
writeIA = (`writeArray` 0)

readIA :: IA -> IO Int
readIA = (`readArray` 0)

newGr :: IO Growable
newGr = do
 rn <- newIA 0
 ra <- newArray_ (0, -1) >>= newIORef
 pure $ Growable rn ra

resetGr :: Growable -> IO ()
resetGr (Growable rn _) = writeIA rn 0

pushGr :: Growable -> Int16 -> IO ()
pushGr (Growable rn ra) x = do
 n <- readIA rn
 a@(IOUArray (STUArray _ _ cap _)) <- readIORef ra
 if n >= cap
 then do
  na <- double a
  writeIORef ra na
  a1 <- readIORef ra
  writeArray a1 n x
 else
  writeArray a n x
 writeIA rn (n + 1)

double :: IOUArray Int Int16 -> IO (IOUArray Int Int16)
double (IOUArray (STUArray _ _ n a#)) = IO $ \s1 -> do
 case n * 2 of
  0 -> case newByteArray# 2# s1 of
   (# s2, a2# #) -> (# s2, IOUArray (STUArray 0 0 1 a2#) #)
  n2@(I# n2#) -> case safe_scale 2# n2# of
   new# -> case resizeMutableByteArray# a# new# s1 of
    (# s2, a2# #) -> (# s2, IOUArray (STUArray 0 (n2 - 1) n2 a2#) #)

releaseGr :: Growable -> IO (IOUArray Int Int16)
releaseGr (Growable rn ra) = do
 n <- readIA rn
 IOUArray (STUArray _ _ _ a#) <- readIORef ra
 pure $ IOUArray (STUArray 0 (n - 1) n a#)

freezeGr :: Growable -> IO (UArray Int Int16)
freezeGr = releaseGr >=> freeze
