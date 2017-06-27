{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Main where

import qualified Data.ByteString.Char8 as B
import qualified System.Random.MWC as MWC
import Utils
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector)
-- import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed.Mutable as MV
import Text.Printf (printf)
import LDA2.Model (prog)
import Control.Monad (forM_)
import Data.List (sort, intercalate)
import Data.Number.LogFloat
import Language.Hakaru.Runtime.LogFloatPrelude
import System.IO
import News
import Debug.Trace
import Control.Monad.ST

data Stats = Stats 
  { z        :: Vector Int
  , zCounts  :: Vector Int
  , wzCounts :: Vector Int
  , dzCounts :: Vector Int
  }
  deriving Show


-- |Step through documents, performing one Gibbs sampling iteration
-- on each to select a new topic. 
-- gibbsRound 
--   :: Vector LogFloat      -- prior probability of each topic
--   -> Vector LogFloat      -- prior probability of each word
--   -> Int                  -- number of documents
--   -> Int                  -- number of words
--   -> Vector Int           -- words, indexed by token position
--   -> Vector Int           -- document, indexed by token position
--   -> Vector Int           -- topics, indexed by document 
--   -> MVector IO Int
--   -> MVector IO Int
--   -> MVector IO Int
--   -> Measure (Vector Int) -- distribution over the updated topic
gibbsRound zPrior wPrior numTopics w d stats = Measure $ \g -> do
    let
      numTokens = V.length $ z stats
      loop i stats = do
        if i == numTokens then return (Just stats)
        else do
          stats' <- gibbsStep g zPrior wPrior numTopics w d i stats
          loop (i + 1) stats'
    loop 0 stats

-- | A single Gibbs step, i.e., update a single variable
gibbsStep g zPrior wPrior numTopics w d i stats = do
  let
    Stats z n_z n_wz n_dz = stats 
    oldZ = z!i
  newZ <- sample g $ prog zPrior wPrior numTopics w d z i n_wz n_dz n_z
  updateStats i numTopics (w!i) (d!i) stats oldZ newZ

updateStats i numTopics word doc stats oldZ newZ = do
  let Stats z n_z n_wz n_dz = stats
  n_wz' <- move n_wz (word*numTopics + oldZ) (word*numTopics + newZ) 
  n_dz' <- move n_dz (doc*numTopics + oldZ) (doc*numTopics + newZ) 
  n_z'  <- move n_z oldZ newZ
  z'    <- inPlace z $ \mz -> MV.write mz i newZ
  return $ Stats z' n_z' n_wz' n_dz'

-- | Move a single count in vector `v` from index `old` to index `new`
move :: Vector Int -> Int -> Int -> IO (Vector Int)
move v old new = inPlace v $ \mv -> do
  MV.modify mv (subtract 1) old
  MV.modify mv (+ 1) new

inPlace v f = do
  mv <- V.unsafeThaw v
  f mv 
  V.unsafeFreeze mv

-- -- |Wrap 'gibbsRound' for simple testing
-- next :: Vector Int -> Measure (Vector Int)
-- next z = gibbsRound zPrior wPrior z w d
--   where
--   r = replicate 5
--   zPrior = V.fromList [1,1]
--   wPrior = V.fromList [1,1,1]
--   w      = V.fromList $ r 0 ++ r 1 ++ r 0 ++ r 2
--   d      = V.fromList $ r 0 ++ r 0 ++ r 1 ++ r 1

-- output :: Vector Int -> IO a
-- output g = iterateM_ f
--   where
--   f z = undefined

counts :: MV.Unbox a => Int -> Vector a -> (a -> Int) -> Vector Int
counts n v f = runST $ do
  t <- MV.replicate n 0
  V.forM_ v $ \x -> MV.modify t (+1) (f x)
  V.unsafeFreeze t

main :: IO ()
main = do
  let corpus = "20_newsgroups"
  (news, enc) <- getNewsL corpus SingleDoc Nothing [1,7]
  -- print news
  let 
    ldacNews = ldac news
    (w,d,topics) = asArrays news
    numWords = V.length w
    numTopics = 10 :: Int
    zPrior = V.fromList . replicate numTopics $ logFloat 1
    wPrior = onesFrom w
    numDocs = 1 + V.maximum d
    z0 = V.fromList . take (V.length w) $ cycle [0..(numTopics-1)]
    n_wz = counts (numWords * numTopics) (V.zip w z0) $ \(w,z) -> w*numTopics + z
    n_dz = counts (numDocs * numTopics) (V.zip d z0) $ \(d,z) -> d*numTopics + z
    n_z  = counts numTopics z0 id
    stats = Stats z0 n_z n_wz n_dz
    next = gibbsRound zPrior wPrior numTopics w d
  writeVec "words" w
  writeVec "docs" d
  writeVec "topics" topics
  withFile "vocab" WriteMode $ \h -> do
    forM_ (reverse $ vocabReverse enc) $ \x -> B.hPutStrLn h x
  withFile "ldac" WriteMode $ \h -> do
    B.hPutStrLn h ldacNews
  hSetBuffering stdout LineBuffering
  g <- MWC.create
  --z0 <- V.replicateM numWords $ MWC.uniformR (0,numTopics - 1) g
  putStrLn . intercalate "," . map show . V.toList $ z stats
  chain g stats next $ \stats -> do
    putStrLn . intercalate "," . map show . V.toList $ z stats
  ---forM_ [0..(V.length topics - 1)] $ \i -> do
    --print $ V.map logFromLogFloat $ predict i
  --  printf "%d, %d\n" (topics ! i) (V.maxIndex $ predict i)
  -- print $ accuracy
  --           topics
  --           (V.map (V.maxIndex . predict) (V.generate (V.length topics) id))
  -- replicateM 5 . withGen g (print . sort) $ do
  --   pred <- predict
  --   let p = pred $ V.fromList $ [0,1,7]
  --   replicateM 100 p


accuracy
    :: V.Vector Int
    -> V.Vector Int
    -> Double
accuracy x y = V.sum z / (fromIntegral $ V.length x)
    where z = V.zipWith (\a b -> if a == b then 1 else 0) x y
