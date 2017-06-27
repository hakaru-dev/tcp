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
gibbsRound zPrior wPrior numTopics w d mn_wz mn_dz mn_z z = Measure $ \g -> do
  -- withFile "z" AppendMode $ \zHandle -> do
    let
      numTokens = V.length z
      loop i mz = do
        if i == numTokens then do
          Just <$> V.unsafeFreeze mz
        else do
          gibbsStep g zPrior wPrior numTopics w d mz i mn_wz mn_dz mn_z
          loop (i + 1) mz
    loop 0 =<< V.unsafeThaw z

gibbsStep g zPrior wPrior numTopics w d mz i mn_wz mn_dz mn_z = do
  z    <- V.freeze mz 
  n_wz <- V.freeze mn_wz
  n_dz <- V.freeze mn_dz
  n_z  <- V.freeze mn_z
  oldZ <- MV.read mz i
  newZ <- sample g $ prog zPrior wPrior numTopics w d z i n_wz n_dz n_z
  updateStats numTopics mn_wz mn_dz mn_z i (d!i) oldZ newZ
  MV.write mz i newZ

updateStats numTopics mn_wz mn_dz mn_z i doc oldZ newZ = do
  move mn_wz (i*numTopics + oldZ) (i*numTopics + newZ) 
  move mn_dz (doc*numTopics + oldZ) (doc*numTopics + newZ) 
  move mn_z oldZ newZ

move mv old new = do
  MV.modify mv (subtract 1) old
  MV.modify mv (+ 1) new


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
  V.freeze t

-- | Outer product
(><) :: Vector Int -> Vector Int -> Vector (Int,Int)
(><) x y = V.fromList [(i,j) | i <- V.toList x, j <- V.toList y]

main :: IO ()
main = do
  let corpus = "20_newsgroups"
  (news, enc) <- getNewsL corpus SingleDoc (Just 10) [1,7]
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
  mn_wz <- V.thaw $ counts (numWords * numTopics) (w >< z0) $ \(w,z) -> w*numTopics + z
  mn_dz <- V.thaw $ counts (numDocs * numTopics) (d >< z0) $ \(d,z) -> d*numTopics + z
  mn_z  <- V.thaw $ counts numTopics z0 id
  let 
    next = gibbsRound zPrior wPrior numDocs w d mn_wz mn_dz mn_z

  -- printf "length zPrior == %d\n" (V.length zPrior)
  -- printf "length wPrior == %d\n" (V.length wPrior)
  -- printf "length words  == %d\n" (V.length words)
  -- printf "length docs   == %d\n" (V.length docs)
  -- printf "length topics == %d\n" (V.length topics)
  
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
  putStrLn . intercalate "," . map show . V.toList $ z0
  chain g z0 (\z -> next z) $ \zs -> do
    putStrLn . intercalate "," . map show . V.toList $ zs
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
