{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GameRunner
 ( searchTo
 , startGame
 ) where

import Control.Logger.Simple
import Control.Monad
import Control.Monad.Reader
import Data.Hashable
import Data.IORef
import Data.List
import Data.Maybe
import Data.Text (append, pack, Text)
import Data.Tree
import Strat.Helpers
import qualified Strat.ZipTree as Z
import Strat.StratIO
import Strat.StratTree.TreeNode
    ( Entry(MoveEntry, CmdEntry),
      Eval,
      FinalState(Draw, WWins, BWins),
      Move,
      Output(showCompMove, updateBoard, out, getPlayerEntry, showAs),
      TreeNode(final, getMove, moveNum, possibleMoves) )
import System.Random hiding (next)
import System.Time.Extra (duration, showDuration, Seconds)
import Text.Printf
-- import Debug.Trace

data RunnerData = RunnerData
    { totalCalcTime :: Seconds
    , avgTimePerMove :: Seconds
    }

class HasRunnerData a where
  runnerData :: a -> IORef RunnerData

data RunnerZipTreeEnv = RunnerZipTreeEnv
  { zipTreeEnv :: Z.ZipTreeEnv
  , runData :: IORef RunnerData
  }

instance Z.HasZipTreeEnv RunnerZipTreeEnv where
  zte = zipTreeEnv

instance HasRunnerData RunnerZipTreeEnv where
  runnerData = runData

-- TODO: make a record/type for all these flags...
startGame :: ( Output o n m, TreeNode n m, Z.ZipTreeNode n, Ord n, Eval n
             , Hashable n, Z.PositionState p)
          => o -> Tree n -> p -> Int -> Int -> Int -> Bool -> Bool -> Bool
          -> Bool -> Bool -> Bool -> Bool -> Bool -> Text -> IO ()
startGame o node startState maxDepth maxCritDepth maxQuietDepth aiPlaysWhite aiPlaysBlack
          enablePreSort enableRandom enablePruning singleThreaded
          enablePruneTracing enableCmpTracing moveTraceStr = do
  let ztEnv = Z.ZipTreeEnv
        { Z.enablePruneTracing
        , Z.enableCmpTracing
        , Z.enableRandom
        , Z.maxRandomChange = 2.0
        , Z.enablePruning
        , Z.enablePreSort
        , Z.moveTraceStr
        , Z.maxDepth
        , Z.maxCritDepth
        , Z.maxQuietDepth
        , Z.aiPlaysWhite
        , Z.aiPlaysBlack
        , Z.singleThreaded
        }
  let rData = RunnerData {totalCalcTime = 0.0, avgTimePerMove = 0.0 }
  ior <- newIORef rData
  let env = RunnerZipTreeEnv { zipTreeEnv = ztEnv, runData = ior }
  startGameLoop env o node

startGameLoop :: ( Output o n m, TreeNode n m, Z.ZipTreeNode n, Ord n, Eval n, Hashable n
                 , Z.HasZipTreeEnv r, HasRunnerData r)
              => r -> o -> Tree n -> IO ()
startGameLoop r o node = do
    let env = Z.zte r
    unless (Z.enablePruning env) $
      logInfo "***** Alpha-Beta pruning is turned OFF *****";
    when (Z.singleThreaded env) $
      logInfo "***** Running SINGLE THREADED *****";
    unless (Z.enablePreSort env) $
      logInfo "***** Pre-eval sorting is turned OFF *****";
    when (Z.enableCmpTracing env) $
      logInfo $ pack $ printf "***** Compare tracing for: |%s| is ON *****" (Z.moveTraceStr env)
    when (Z.enablePruneTracing env) $
      logInfo $ pack $ printf "***** Prune tracing for: |%s| is ON *****" (Z.moveTraceStr env)
    logInfo $ "critDepth: " `append` pack (show (Z.maxCritDepth env))
    logInfo $ "quietDepth: " `append` pack (show (Z.maxQuietDepth env))
    rnd <- if Z.enableRandom env
             then
               Just <$> getStdGen
             else do
               liftIO $ logInfo "***** Random move selection: OFF *****"
               return (Nothing :: Maybe StdGen)
    _ <- liftIO $ runReaderT
      ( do
        -- TODO: Test this with 1 1
        newTree <- expandToSingleThreaded node 2 2
        liftIO $ logInfo $ "(startGameLoop - treesize: " `append` pack (show (Z.treeSize newTree))
          `append` pack ")"
        loop rnd o newTree []
      ) r
    return ()

moveHistory :: TreeNode n m => [n] -> [m]
moveHistory tns = getMove <$> tns

loop :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, Ord n, Eval n, Hashable n,
         RandomGen g, Z.HasZipTreeEnv r, HasRunnerData r)
     => Maybe g -> o -> Tree n -> [n] -> Z.ZipTreeM r ()
loop gen o node nodeHistory = do
    let label = rootLabel node
    liftIO $ updateBoard o label
    (theNext, updatedHistory) <- case final label of
        WWins -> do
            liftIO $ out o "White wins."
            return (Nothing, nodeHistory)
        BWins -> do
            liftIO $ out o "Black wins."
            return (Nothing, nodeHistory)
        Draw -> do
            liftIO $ out o "Draw."
            return (Nothing, nodeHistory)
        _ -> do
            (nextNode, h) <- do
                bCompTurn <- isCompTurn (Z.ztnSign label)
                if bCompTurn
                  then computersTurn gen o node nodeHistory
                  else playersTurn gen o node nodeHistory
            return (Just nextNode, h)
    case theNext of
        Nothing -> return ()
        Just next -> loop gen o next updatedHistory

-- TODO: remove this exclusions parameter once its clear it is not needed
-- TODO: remove maxDepth params once its clear that 'exclusions'list is not needed
playersTurn :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, Hashable n, RandomGen g, Z.HasZipTreeEnv r)
           => Maybe g -> o -> Tree n -> [n] -> Z.ZipTreeM r (Tree n, [n])
playersTurn gen o t nodeHistory = do
    -- populate 1 deep just so findMove can work with player vs player games
    -- TODO: skip this when not needed
    (expandedT, _result) <- searchToSingleThreaded t (Nothing :: Maybe StdGen) 1 1
    entry <- liftIO $ getPlayerEntry o expandedT []
    logInfo $ "entered by player: " `append` pack (show entry)
    case entry of
      CmdEntry s -> do
        result <- processCommand s (rootLabel t) nodeHistory o
        case result of
          -- TODO: - expand simplifying assumption that 'Just _' can only mean 'undo' in player vs computer game...
          Just (t', nodeHistory') -> do
            let label = rootLabel t'
            liftIO $ updateBoard o label
            -- fill out a minimal search tree from the undo point:
            (expandedUndo, _) <- searchToSingleThreaded t' (Nothing :: Maybe StdGen) 2 2
            playersTurn gen o expandedUndo nodeHistory'
          Nothing -> playersTurn gen o t nodeHistory
      MoveEntry mv ->
        case findMove expandedT mv of
          Right newTree -> return (newTree, rootLabel newTree:nodeHistory)
          Left s ->  do
              liftIO $ putStrLn s
              let newNodeMoves = possibleMoves (rootLabel expandedT)
              liftIO $ putStrLn $ "Available moves:" ++ show newNodeMoves
              playersTurn gen o t nodeHistory

computersTurn :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, Hashable n, Ord n,
                  Eval n, RandomGen g, Z.HasZipTreeEnv r, HasRunnerData r)
              => Maybe g-> o -> Tree n -> [n] -> Z.ZipTreeM r (Tree n, [n])
computersTurn gen o t nodeHistory = do
    r <- ask
    (sec, (newRoot, updatedHistory, isCrit)) <- duration $ do
        (expandedT, result) <- searchTo t gen
        liftIO $ putStrLn "\n--------------------------------------------------\n"
        liftIO $ showCompMove o expandedT result True
        let nextNode = Z.nmNode (Z.picked result)
        let nextMove = getMove nextNode
        let crit =
              let path = Z.nmMovePath (Z.picked result)
                  lst = last path -- path is never empty
              in Z.ztnDeepDescend lst
        return (findMove expandedT nextMove, nextNode:nodeHistory, crit)
    liftIO $ logInfo $ "Computer move time: " `append` pack (showDuration sec)
    let nMoves = moveNum $ rootLabel t
    let divisor :: Double = fromIntegral ((nMoves `div` 2) + 1)
    avgTime <- liftIO $ atomicModifyIORef' (runnerData r) (\RunnerData{..} ->
        let newTotal = totalCalcTime + sec
            newAvg = newTotal / divisor
        in ( RunnerData { totalCalcTime = newTotal
                        , avgTimePerMove = newAvg }
           , newAvg))
    liftIO $ logInfo $ pack $ printf "nMoves: %d, divisor %f" nMoves divisor
    liftIO $ putStrLn $ "Average computer move time: " ++ showDuration avgTime
    liftIO $ logInfo $ "Average computer move time: "  `append` pack (showDuration avgTime)
        `append` pack "\n"
    case newRoot of
        Right r -> do
          --TODO: is TreeNode.critical being used?
          when isCrit $ do
              liftIO $ putStrLn "Warning computer move NOT quiescent! (remove this message)"
          return (r, updatedHistory)
        Left s -> do
          let newNodeMoves = possibleMoves (rootLabel t)
          liftIO $ putStrLn $ "Available moves:" ++ show newNodeMoves
          error s

isCompTurn :: (Z.HasZipTreeEnv r) => Z.Sign -> Z.ZipTreeM r Bool
isCompTurn sign = do
    r <- ask
    let env = Z.zte r
    let aiPlaysWhite = Z.aiPlaysWhite env
    let aiPlaysBlack = Z.aiPlaysBlack env
    return $
      case sign of
        Z.Pos
          | aiPlaysWhite -> True
          | otherwise -> False
        Z.Neg
          | aiPlaysBlack -> True
          | otherwise -> False

-- TODO: the depth and critDepth from the env and remove the two parameters
searchTo :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g, Z.HasZipTreeEnv r)
         => Tree n -> Maybe g -> Z.ZipTreeM r (Tree n, Z.NegaResult n)
searchTo t gen = do
    r <- ask
    let env = Z.zte r
    let depth = Z.maxDepth env
    let critDepth = Z.maxCritDepth env
    let quietDepth = Z.maxQuietDepth env
    if Z.singleThreaded env
        then searchToSingleThreaded t gen depth critDepth
        else searchToMultiThreaded t gen depth critDepth quietDepth

processUndo :: (TreeNode n m, Z.HasZipTreeEnv r) => [n] -> Z.ZipTreeM r (Maybe (Tree n, [n]))
processUndo ns = do
  liftIO $ logInfo $ "processUndo - length of list: "  `append` pack (show (length ns))
  case ns of
    ns
      | len <- length ns
      , len < 3
      -> do
        liftIO $ putStrLn "Undo not yet available"
        return Nothing
      | otherwise -> do
        let _x:_y:z:zs = ns
        liftIO $ putStrLn "Undoing the last move of each side ..."
        liftIO $ logInfo "Undoing the last move of each side ..."
        return $ Just (Node z [], z:zs)

processCommand :: (TreeNode n m, Move m, Z.ZipTreeNode n, Hashable n, Output o n m, Z.HasZipTreeEnv r)
                => String -> n -> [n] -> o -> Z.ZipTreeM r (Maybe (Tree n, [n]))
processCommand cmd node nodeHistory o
    | cmd == "hash" = do
        let str = "hash of current position: " ++ show (Z.nodeHash node)
        liftIO $ putStrLn str
        liftIO $ logInfo $ pack str
        return Nothing
    | cmd == "fen" = do
        liftIO $ showAs o "FEN" node
        return Nothing
    | cmd == "list" = do
        liftIO $ putStrLn $ intercalate "\n" (show <$> reverse (moveHistory nodeHistory))
        return Nothing
    | cmd == "undo" = do
        processUndo nodeHistory
    | cmd == "moves" = do
        let str = show $ possibleMoves node
        liftIO $ putStrLn str
        liftIO $ logInfo $ pack str
        return Nothing
    | lvl <- strToLevelCmd cmd
    , isJust lvl = do
        let level = fromJust lvl
        let msg = "Setting log level to: " ++ show level
        liftIO $ putStrLn msg
        liftIO $ logInfo $ pack msg
        liftIO $ setLogLevel level
        return Nothing
    | otherwise = do
        liftIO $ putStrLn $ "Unhandled Commandi!: " ++ cmd
        return Nothing

strToLevelCmd :: String -> Maybe LogLevel
strToLevelCmd s
  | s == show LogTrace = Just LogTrace
  | s == show LogDebug = Just LogDebug
  | s == show LogInfo  = Just LogInfo
  | s == show LogNote  = Just LogNote
  | s == show LogWarn  = Just LogWarn
  | s == show LogError = Just LogError
  | otherwise                   = Nothing
