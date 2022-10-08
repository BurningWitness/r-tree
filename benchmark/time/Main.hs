{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import           Data.RTree.MBB
import           Data.RTree (RTree, MBB)
import qualified Data.RTree as R

import           Control.DeepSeq
import           Control.Monad
import           Data.Foldable
import           Data.List hiding (lookup, map)
import           Data.Monoid
import           Gauge
import           Prelude hiding (lookup, map)
import           System.Random.Stateful



instance NFData MBB where
  rnf !_ = ()

randPoint :: StatefulGen g m => g -> m MBB
randPoint g = do
  a <- uniformRM (0, 2 ^ (20 :: Int)) g
  b <- uniformRM (0, 2 ^ (20 :: Int)) g
  return $ MBB a b (a + 1) (b + 1)

randArea :: StatefulGen g m => g -> m MBB
randArea g = do
  a <- uniformRM (0, 2 ^ (20 :: Int)) g
  b <- uniformRM (0, 2 ^ (20 :: Int)) g
  c <- uniformRM (0, 2 ^ (20 :: Int)) g
  d <- uniformRM (0, 2 ^ (20 :: Int)) g
  return $ MBB (a `min` c) (b `min` d) (a `max` c) (b `max` d)



newStdGenM :: IO (IOGenM StdGen)
newStdGenM = newIOGenM $ mkStdGen 0

genPoints :: StatefulGen g m => Int -> g -> m [(MBB, Int)]
genPoints n g = flip zip [0..] <$> replicateM n (randPoint g)

genAreas :: StatefulGen g m => Int -> g -> m [MBB]
genAreas n = replicateM n . randPoint



main :: IO ()
main = do
  defaultMain
    [ env ( do g <- newIOGenM $ mkStdGen 0
               no <- genPoints 4096 g
               return no
          ) $ \ ~raw ->
        bgroup "insert"
          [ bench "Gut" $
              nf (foldl' (flip $ uncurry R.insert) R.empty) raw
          , bench "fromList" $
              nf R.fromList raw
          ]
    
    , env ( do g <- newIOGenM $ mkStdGen 0
               no <- genPoints 4096 g
               return (R.fromList no, fst <$> no)
          ) $ \ ~(r, brs) ->
        bench "delete" $
          nf (foldr R.delete r) brs

    , env ( do g <- newIOGenM $ mkStdGen 0
               no <- genPoints 4096 g
               return (R.fromList no, take 1024 $ fst <$> no)
          ) $ \ ~(r, brs) ->
        bgroup "lookups"
          [ bench "lookup" $
              nf (foldMap $ \bx -> [R.lookup bx r]) brs
          , bench "intersect" $
              nf (foldMap $ \bx -> [R.intersect bx r]) brs
          , bench "contain" $
              nf (foldMap $ \bx -> [R.lookupRange bx r]) brs
          , bench "within" $
              nf (foldMap $ \bx -> [R.lookupContainsRange bx r]) brs
          ]

    ]
