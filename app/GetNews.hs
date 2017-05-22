module Main where

import qualified Data.ByteString.Char8 as B
import News
import qualified System.Random.MWC as MWC
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))
import Text.Printf (printf)
import Control.Monad (forever, replicateM, forM_)
import Data.List (sort)
import Data.Number.LogFloat
import System.IO
import Utils


main = do
  ((words, docs, topics), enc) <- getNews SingleDoc Nothing [0..]
  writeVec "words" words
  writeVec "docs" docs
  writeVec "topics" topics
  withFile "vocab" WriteMode $ \h -> do
    forM_ (reverse $ vocabReverse enc) $ \x -> B.hPutStrLn h x