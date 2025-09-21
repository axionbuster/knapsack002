{-# LANGUAGE TypeApplications #-}
module Main (main) where
import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Array.IO
import           Data.Array.Unboxed
import           Data.Array.Unsafe
import qualified Data.ByteString.Builder as B
import           Data.List               (intersperse)
import           GHC.Conc
import           Knap2
import           Parse
import           System.IO

main :: IO ()
main = do
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
