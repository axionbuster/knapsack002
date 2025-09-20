{-# LANGUAGE TypeApplications #-}
module Main (main) where
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Array.IO
import           Data.Array.Unboxed
import           Data.Array.Unsafe
import qualified Data.ByteString.Builder   as B
import qualified Data.ByteString.Char8     as B
import           Data.Int
import           Data.List                 (intersperse)
import           GHC.Conc
import           Knap
import           System.IO

io :: (MonadIO m) => IO a -> m a
io = liftIO
{-# INLINE io #-}

nextPair :: (Monad m) => StateT B.ByteString m (Maybe (Int16, Int16))
nextPair = do
 let fi = fromIntegral
     {-# INLINE fi #-}
 bs0 <- get
 case B.readInt bs0 of
  Just (n0, bs1) -> case B.readInt (B.drop 1 bs1) of
   Just (n1, bs2) -> do put (B.drop 1 bs2); pure $ Just (fi n0, fi n1)
   Nothing        -> pure Nothing
  Nothing -> pure Nothing

main :: IO ()
main = do
 let
  work :: StateT B.ByteString IO [Thought B.Builder]
  work = do
   capnum <- nextPair
   case capnum of
    Just (cap, num) -> do
     dvs <- io $ newArray_ @IOUArray (0, num - 1)
     dws <- io $ newArray_ @IOUArray (0, num - 1)
     let
      rep i | i == num = do
       vs <- io $ unsafeFreeze @_ @_ @_ @_ @UArray dvs
       ws <- io $ unsafeFreeze @_ @_ @_ @_ @UArray dws
       let
        -- 0x0a = line feed; 0x20 = space
        k = knap
         (\n -> B.int16Dec n <> B.word8 0x0a) -- announce count
         (\j -> B.int16Dec j <> B.word8 0x20) -- announce index
         cap vs ws
       k `par` ((k :) <$> work)
      rep i = do
       vw <- nextPair
       case vw of
        Just (v, w) -> do
         io $ writeArray dvs i v
         io $ writeArray dws i w
         rep (i + 1)
        Nothing -> fail "premature termination"
     rep 0
    Nothing -> pure mempty
 B.getContents >>= evalStateT work >>=
  B.hPutBuilder stdout .
  mconcat .
  intersperse (B.word8 0x0a) .
  map think
