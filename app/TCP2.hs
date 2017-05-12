module Main where

import qualified Data.ByteString.Char8 as B
import News (getNews)
import qualified System.Random.MWC as MWC
import Utils
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector
import Data.Vector.Unboxed (Vector)
-- import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed.Mutable as MV
import Text.Printf (printf)
import LDA.Model (prog)
import Control.Monad (forever, replicateM, forM_)
import Data.List (sort)
import Data.Number.LogFloat
import Language.Hakaru.Runtime.LogFloatPrelude


-- |Step through documents, performing one Gibbs sampling iteration
-- on each to select a new topic. 
gibbsRound 
     :: Vector LogFloat        -- prior probability of each topic
     -> Vector LogFloat        -- prior probability of each word
     -> Vector Int           -- words, indexed by token position
     -> Vector Int           -- document, indexed by token position
     -> Int                  -- number of documents
     -> Vector Int           -- topics, indexed by document 
     -> Measure (Vector Int) -- distribution over the updated topic
gibbsRound zPrior wPrior w d nd z = Measure $ \g -> do
  let
    numTokens = V.length z
    loop i mz = 
      if i == numTokens then Just <$> V.unsafeFreeze mz
      else do
        z <- V.unsafeFreeze mz
        maybeTopic <- unMeasure (prog zPrior wPrior z w d nd i) g
        case maybeTopic of
          Nothing -> return Nothing
          Just topic -> do
            mz' <- V.unsafeThaw z
            MV.write mz' i topic
            loop (i + 1) mz'
  loop 0 =<< V.thaw z

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

main = do
  ((w, d, topics), enc) <- getNews (Just 1) [0..4]
  g <- MWC.create
  let 
    numTopics = 5 :: Int
    zPrior = V.fromList . replicate numTopics $ logFloat 1
    wPrior = onesFrom w
    numDocs = 1 + V.maximum d
    z0 = V.fromList . take (V.length w) $ cycle [1..numTopics]
    next = gibbsRound zPrior wPrior w d numDocs
  -- printf "length zPrior == %d\n" (V.length zPrior)
  -- printf "length wPrior == %d\n" (V.length wPrior)
  -- printf "length words  == %d\n" (V.length words)
  -- printf "length docs   == %d\n" (V.length docs)
  -- printf "length topics == %d\n" (V.length topics)
  chain g z0 next print
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
