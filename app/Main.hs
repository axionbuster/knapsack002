module Main (main) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import qualified Data.ByteString.Builder   as B
import qualified Data.ByteString.Char8     as B
import           GHC.Conc
import           Growable
import           Knap
import           System.IO

io :: (MonadIO m) => IO a -> m a
io = liftIO

nextPair :: (Monad m) => StateT B.ByteString m (Maybe (Int, Int))
nextPair = do
 bs0 <- get
 case B.readInt bs0 of
  Just (n0, bs1) -> case B.readInt (B.drop 1 bs1) of
   Just (n1, bs2) -> do put (B.drop 1 bs2); pure $ Just (n0, n1)
   Nothing        -> pure Nothing
  Nothing -> pure Nothing

main :: IO ()
main = do
 gvs <- newGr
 gws <- newGr
 let
  work :: StateT B.ByteString IO B.Builder
  work = do
   capnum <- nextPair
   case capnum of
    Just (cap, num) -> do
     let
      rep i | i == num = do
       vs <- io $ freezeGr gvs; io $ resetGr gvs
       ws <- io $ freezeGr gws; io $ resetGr gws
       let
        -- 0x0a = line feed; 0x20 = space
        k = knap
         (\n -> B.intDec n <> B.word8 0x0a) -- announce count
         (\j -> B.intDec j <> B.word8 0x20) -- announce index
         cap vs ws <> B.word8 0x0a
       k `par` ((k <>) <$> work)
      rep i = do
       vw <- nextPair
       case vw of
        Just (v, w) -> do
         io $ pushGr gvs $ fromIntegral v
         io $ pushGr gws $ fromIntegral w
         rep (i + 1)
        Nothing -> fail "premature termination"
     rep 0
    Nothing -> pure mempty
 B.getContents >>= evalStateT work >>= B.hPutBuilder stdout
