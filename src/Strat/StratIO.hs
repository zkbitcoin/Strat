{-# language GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Strat.StratIO
  ( evalTreeSingleThreaded
  , evalTreeMultiThreaded
  , expandToSingleThreaded
  , expandToParallel
  , negaMaxParallel
  , negaMaxSingleThreaded
  , searchToSingleThreaded
  , searchToMultiThreaded
  ) where

import qualified Control.Concurrent.Async as Async
import Control.Exception (assert)
import Control.Logger.Simple ( logInfo )
import Control.Monad.Reader
import qualified Data.List as List (delete)
import Data.Hashable
import Data.Text (pack, append)
import Data.Tree (Tree)
import qualified Data.Tree as T
import Data.Tree.Zipper
import Strat.ZipTree hiding (expandTo)
import qualified Strat.ZipTree as Z
import Strat.StratTree.TreeNode
import Text.Printf
import System.Random

searchToSingleThreaded
    :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g, Z.HasZipTreeEnv r)
    => Tree n -> Maybe g -> Int -> Int -> Z.ZipTreeM r (Tree n, Z.NegaResult n)
searchToSingleThreaded t gen maxDepth maxCritDepth = do
    expanded <- expandToSingleThreaded t maxDepth maxCritDepth
    evalTreeSingleThreaded expanded gen

searchToMultiThreaded :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g, Z.HasZipTreeEnv r)
         => Tree n -> Maybe g -> Int -> Int -> Int
         -> Z.ZipTreeM r (Tree n, Z.NegaResult n)
searchToMultiThreaded initialTree gen maxDepth maxCritDepth maxQuietDepth = do
  let loop tree g maxD maxCD = do
        expanded <- expandToParallel tree maxD maxCD
        result <- evalTreeMultiThreaded expanded g
        case result of
          Right (t, nr) -> return (t, nr)
          Left (t, nr) -> do
            if maxCD == maxQuietDepth
              then do
                let s = printf "***** Warning - reached maxQuietDepth (%d) without a quiet move." maxCD
                liftIO $ putStrLn s
                liftIO $ logInfo $ pack s
                return (t, nr)
              else do
                let s = printf "***** expanding the search to level: %d" (maxCD + 1)
                liftIO $ putStrLn s
                liftIO $ logInfo $ pack s
                loop t g maxD (maxCD + 1)
  loop initialTree gen maxDepth maxCritDepth

evalTreeSingleThreaded :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g, Z.HasZipTreeEnv r)
         => Tree n -> Maybe g -> Z.ZipTreeM r (Tree n, Z.NegaResult n)
evalTreeSingleThreaded t gen = do
    r <- ask
    let env = Z.zte r
    res <- negaMaxSingleThreaded env t gen
    return (t, res)

evalTreeMultiThreaded :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g, Z.HasZipTreeEnv r)
         => Tree n -> Maybe g -> Z.ZipTreeM r (Either (Tree n, Z.NegaResult n) (Tree n, Z.NegaResult n))
evalTreeMultiThreaded t gen = do
    r <- ask
    let env = Z.zte r
    res <- negaMaxParallel env t gen
    case res of
        Left r -> return $ Left (t, r)
        Right r -> return $ Right (t, r)


expandToParallel :: (Ord a, Show a, ZipTreeNode a, HasZipTreeEnv r)
                 => T.Tree a -> Int -> Int -> Z.ZipTreeM r (T.Tree a)
expandToParallel t depth critDepth = do
    -- expansion of at least one level should always have been previously done
    let (tSize, tLevels)  = treeSize t
    liftIO $ logInfo $ "Tree size: " `append` pack (show tLevels)
    let _num_levels = assert (length tLevels >= 2) (length tLevels)
    let theChildren = T.subForest t
    liftIO $ logInfo $ pack $ printf "expandToParallel -- number of threads that will be created: %d" (length theChildren)
    liftIO $ logInfo $ pack $ printf "(expansion to depth:%d, critDepth %d)" depth critDepth
    env <- ask
    newChildren <- liftIO $ Async.forConcurrently theChildren
      (\x -> runReaderT (Z.expandTo x 2 depth critDepth) env)
    let z = fromTree t
    let newTree = toTree $ modifyTree (\(T.Node x _) -> T.Node x newChildren) z
    return newTree

expandToSingleThreaded :: forall a r p. (Ord a, Show a, ZipTreeNode a, HasZipTreeEnv r)
                       => T.Tree a -> Int -> Int -> Z.ZipTreeM r (T.Tree a)
expandToSingleThreaded t depth critDepth = do
  liftIO $ logInfo $ pack $ printf "expandSingleThreaded called with depth:%d, critDepth:%d" depth critDepth
  Z.expandTo t 1 depth critDepth

negaMaxParallel :: (Ord a, Show a, ZipTreeNode a, Hashable a, RandomGen g, HasZipTreeEnv r)
        => Z.ZipTreeEnv -> T.Tree a -> Maybe g -> Z.ZipTreeM r (Either (NegaResult a) (NegaResult a))
negaMaxParallel env t gen = do
    let theChildren = T.subForest t
    resultsList <- liftIO $ Async.forConcurrently theChildren (\x -> do
        (threadTC, threadEC) <- runReaderT (Z.negaWorker x) env
        let threadNode = T.rootLabel x
        return ( threadTC { node = threadNode , movePath = movePath threadTC ++  [threadNode] }
                 , threadEC))
    let sign = ztnSign $ T.rootLabel t
    let initTC = if sign == Pos then Min else Max
    let curried = foldf sign
    let (theBestTC, ec::Int) = foldr curried (initTC, 0) resultsList
    let curriedAltf = foldfAlts sign (maxRandomChange env) theBestTC
    let theAlts = foldr (curriedAltf . fst) [] resultsList
    let theBestChoices = theBestTC : theAlts
    let allChoices = fst <$> resultsList
    case Z.pickQuietMove gen sign theBestChoices allChoices of
        Right tc -> do
            let notPicked = List.delete tc theBestChoices
            let picked = toNegaMoves tc
            return $ Right $ NegaResult { picked
                                      , bestScore = toNegaMoves theBestTC
                                      , alternatives = toNegaMoves <$> notPicked
                                      , evalCount = ec }
        Left (tc, s) -> do
            liftIO $ putStrLn s
            liftIO $ logInfo $ pack s
            let notPicked = List.delete tc theBestChoices
            let picked = toNegaMoves tc
            return $ Left $ NegaResult { picked
                                      , bestScore = toNegaMoves theBestTC
                                      , alternatives = toNegaMoves <$> notPicked
                                      , evalCount = ec }
    where
        foldf :: forall a. (Ord a, Show a, ZipTreeNode a, Hashable a)
              => Sign
              -> (TraceCmp a, Int)
              -> (TraceCmp a, Int)
              -> (TraceCmp a, Int)
        foldf Pos (tc, numEvals) (tcAcc, numEvalsAcc) =
          (maxTC tcAcc tc, numEvalsAcc + numEvals)
        foldf Neg (tc, numEvals) (tcAcc, numEvalsAcc) =
          (minTC tcAcc tc, numEvalsAcc + numEvals)

        foldfAlts :: forall a. (Ord a, Show a, ZipTreeNode a, Hashable a)
                  => Sign -> Float -> TraceCmp a -> TraceCmp a -> [TraceCmp a] -> [TraceCmp a]
        foldfAlts sgn maxRnd tcBest x acc =
          if isWithin sgn maxRnd tcBest x
            then x : acc
            else acc

negaMaxSingleThreaded :: forall a g r. (Ord a, Show a, ZipTreeNode a, Hashable a, RandomGen g)
                       => ZipTreeEnv -> T.Tree a -> Maybe g -> Z.ZipTreeM r (NegaResult a)
negaMaxSingleThreaded env t gen = do
    Z.negaMax t gen
