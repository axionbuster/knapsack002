{-# LANGUAGE FlexibleContexts #-}
import           Control.Monad
import           Control.Monad.RWS
import           Control.Monad.State
import           Data.Bits
import qualified Data.ByteString.Builder as B
import           Data.Tuple
import           Data.Word
import           Numeric
import           System.Environment
import           System.Exit
import           System.IO

data Xo = Xo !Word64 !Word64 !Word64 !Word64

-- https://prng.di.unimi.it/xoshiro256plusplus.c
xoshiro :: Xo -> (Word64, Xo)
xoshiro (Xo a b c d) =
 let a' = a `xor` d'; b' = b `xor` c'
     c' = c `xor` a ; d' = d `xor` b
  in (rotate (a + d) 23 + a, Xo a' b' (c' `xor` b .<<. 17) (rotate d' 45))

-- https://stackoverflow.com/questions/11641629/generating-a-uniform-distribution-of-integers-in-c
unif :: (Word64, Word64) -> Xo -> (Word64, Xo)
unif (l, u) = runState go where
 len = u - l + 1
 bnd = maxBound - maxBound `rem` len
 go = do
  x <- state xoshiro
  if x >= bnd
  then go
  else pure $ l + x `rem` len

-- result of expo > 0
-- https://en.wikipedia.org/wiki/Geometric_distribution#Random_variate_generation
expo :: Word64 -> Double -> Xo -> (Word64, Xo)
expo up ip = runState go where
 go = do
  x <- state xoshiro
  let u = fromIntegral x / fromIntegral (maxBound :: Word64)
  -- Herbie-optimized version of: 1 + ceil[ln(1-u)/ln(1-1/ip)]
  let y = 1 + ceiling (negate (u / log1p (negate (recip ip))))
  if y > up then go else pure y

case1 :: Double -> Xo -> (B.Builder, Xo)
case1 granularity = swap . execRWS entry () where
 pair x y = tell $
  B.word64Dec x <> B.word8 0x20 <> B.word64Dec y <> B.word8 0x0a
 entry = do
  nentry <- state $ unif (1, 2000)
  capaci <- state $ unif (1, 2000)
  pair nentry capaci
  let
   go i = when (i < nentry) $ do
    value  <- state $ expo 10000 granularity
    weight <- state $ expo 10000 granularity
    pair value weight
    go (i + 1)
  go 0

main :: IO ()
main = do
 let
  granbad gran =
   "granularity of " ++ gran ++
   " is either illegible or outside of the valid range of [1, 10000]"
  help = "\nhelp: (program) (seed: Word64) [granularity: Double(1..10000)]."
 (seed, gran) <- do
  args <- getArgs
  case args of
   [seed] -> pure (read seed, 1234)
   [seed, gran]
    | g <- read gran, 1 <= g, g <= 10000 -> pure (read seed, g)
    | otherwise -> die $ granbad gran ++ "\narguments = " ++ show args ++ help
   [] -> die $ drop 1 help
   xs -> die $ "too many arguments: " ++ show xs ++ help
 let initial = Xo seed seed seed seed
 B.hPutBuilder stdout $ (`evalState` initial) $ do
  ncases <- state $ unif (1, 30)
  let
   go i | i < ncases = (<>) <$> state (case1 gran) <*> go (i + 1)
   go _ = pure mempty
  go 0
