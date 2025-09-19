import System.Environment (getArgs)
import Control.Monad (forM_)
import Data.List (foldl')

readInts :: String -> [Int]
readInts = map read . words

data Case = Case { cap :: Int, items :: [(Int,Int)] } deriving Show

parseCases :: [Int] -> ([Case],[Int])
parseCases [] = ([],[])
parseCases (c:n:rest) =
  let (pairs, rest') = splitAt (2*n) rest
      itms = toPairs pairs
      (more, rest'') = parseCases rest'
  in (Case c itms : more, rest'')
parseCases xs = ([], xs)

toPairs :: [Int] -> [(Int,Int)]
toPairs [] = []
toPairs (a:b:xs) = (a,b):toPairs xs
toPairs _ = []

-- outputs: [count, indices...] per case (two lines per case)
parseOut :: String -> [[Int]]
parseOut s = go (lines s) where
  go [] = []
  go (l1:l2:ls) = let _cnt = read l1 :: Int
                      idxs = readInts l2
                  in idxs : go ls
  go _ = []

sumFor :: [(Int,Int)] -> [Int] -> (Int,Int)
sumFor it = foldl' step (0,0) where
  arr = it  -- solver outputs 0-based indices
  step (sv,sw) i = let (v,w) = arr !! i in (sv+v, sw+w)

main :: IO ()
main = do
  args <- getArgs
  let [inp, oldOut, newOut] = case args of
        [a,b,c] -> [a,b,c]
        _ -> ["tmp/large01.in","tmp/large01.out","tmp/large01.new.out"]
  sIn <- readFile inp
  let ints = readInts sIn
  let (cases, _) = parseCases ints
  old <- fmap parseOut (readFile oldOut)
  new <- fmap parseOut (readFile newOut)
  let trip = zip3 [1..] cases (zip old new)
  forM_ trip $ \(ci, Case c it, (idxOld, idxNew)) -> do
    let (vO,wO) = sumFor it idxOld
    let (vN,wN) = sumFor it idxNew
    putStrLn $ "Case "++show ci++": cap="++show c
    putStrLn $ "  old k="++show (length idxOld)++", V="++show vO++", W="++show wO
    putStrLn $ "  new k="++show (length idxNew)++", V="++show vN++", W="++show wN
    let okO = wO <= c
    let okN = wN <= c
    putStrLn $ "  feasible? old="++show okO++" new="++show okN
    putStrLn $ "  delta V(new-old)="++show (vN - vO)
