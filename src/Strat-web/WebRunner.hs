{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}

module WebRunner where

import StratTree.TreeNode
import StratTree.StratTree
import StratIO.StratIO
import Data.Tree
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Lens
import Data.Aeson
import qualified CheckersJson as J
import qualified Checkers as Ck

--TODO: fix -- to get things working, for the moment this web piece is checkers specific

--TODO: move to common...
gameEnv :: Env
gameEnv = Env {_depth = 6, _errorDepth = 4, _equivThreshold = 0, _errorEquivThreshold = 0,
     _p1Comp = False, _p2Comp = True}

data Jsonable = forall j. ToJSON j => Jsonable j 

data NodeWrapper = NodeWrapper {getNode :: Tree Ck.CkNode, getLastMove :: Maybe Ck.CkMove
                              , getJsonable :: Jsonable}

--Note: The node here already incorporates the move -- the move is included for debugging, etc.
-- data ComputerMove = ComputerMove{getNode :: Tree Ck.CkNode, getMove Ck.CkMove}

----------------------------------------------------------------------------------------------------
-- Exported functions
----------------------------------------------------------------------------------------------------         
--start game request received  
processStartGame :: Tree Ck.CkNode -> Bool -> IO NodeWrapper
processStartGame node bComputerResponse = 
    if bComputerResponse
        then computerResponse node 1 
        else return $ createUpdate "New Game, player moves first" node Nothing    

processComputerMove :: Tree Ck.CkNode -> IO NodeWrapper
processComputerMove tree = do
    let posColor = rootLabel tree ^. Ck.ckPosition . Ck.clr
    liftIO $ putStrLn $ "Computer move (In processComputerMove), turn = " ++ show (colorToTurn posColor)
    computerResponse tree (colorToTurn posColor)
      
 
--player move (web request) received
processPlayerMove :: Tree Ck.CkNode -> Ck.CkMove -> Bool -> IO NodeWrapper
processPlayerMove tree mv bComputerResponse = do
    let processed = processMove tree mv
    let done = checkGameOver processed
    if fst done
        then return $ createMessage (snd done) processed
        else if bComputerResponse
            then do 
                let posColor = rootLabel processed ^. (Ck.ckPosition . Ck.clr)
                liftIO $ putStrLn $ "Computer move (In processPlayerMove), turn = " ++ show (colorToTurn posColor)
                computerResponse processed (colorToTurn posColor) 
            else return $ createUpdate "No computer move" processed Nothing
 
processMessage :: String -> Tree Ck.CkNode -> IO NodeWrapper
processMessage str tree = return $ createMessage str tree

processError :: String -> Tree Ck.CkNode -> IO NodeWrapper
processError str tree = return $ createError str tree
 
----------------------------------------------------------------------------------------------------
 -- Internal functions
----------------------------------------------------------------------------------------------------  11
   
computerResponse :: Tree Ck.CkNode -> Int -> IO NodeWrapper
computerResponse tree turn = do
    eitherNode <- computerMove tree turn
    case eitherNode of
        Left s -> return $ createError s tree
        Right (n, m) -> return $ createUpdate (snd (checkGameOver n)) n (Just m)      
        
computerMove :: Tree Ck.CkNode -> Int -> IO (Either String (Tree Ck.CkNode, Ck.CkMove))
computerMove node turn = do
    let newTree = evalState (runReaderT (unRST (expandTree node)) gameEnv) (GameState 0)
    let resultM = evalState (runReaderT (unRST (best newTree (turnToColor turn))) gameEnv) (GameState 0)
    case resultM of
        Nothing -> return $ Left "Invalid result returned from best"
        Just result -> do
            --let badMovesM = checkBlunders newTree (turnToColor turn) (result^.moveScores)
            --let badMoves = evalState (runReaderT (unRST badMovesM) gameEnv) (GameState 0)
            --let finalChoices = fromMaybe (result ^. moveScores) badMoves
            
            --moveM <- resolveRandom finalChoices
            moveM <- resolveRandom (result^.moveScores)
            case moveM of
                Nothing -> return $ Left "Invalid result from resolveRandom"
                Just mv -> return $ Right ((processMove newTree mv), mv)
                                                   
compMoveNext :: Tree Ck.CkNode -> Int -> Bool
compMoveNext _ turn = evalState (runReaderT (unRST (isCompTurn turn)) gameEnv) (GameState 0)                

checkGameOver :: Tree Ck.CkNode -> (Bool, String)
checkGameOver node =              
    case final $ rootLabel node of
        WWins -> (True, "White wins.")
        BWins -> (True, "Black wins.")
        Draw  -> (True, "Draw.")
        _     -> (False, "Message TBD")
                                                                                                      
isCompTurn :: Int -> RST Bool
isCompTurn turn = do
    p1 <- asks _p1Comp
    p2 <- asks _p2Comp
    return $ if turn == 1 then p1 else p2

--toBool :: "C" or "c" for computer -> True, "H" or "h" (or anything else for that matter) for Human -> False
toBool :: String -> Bool
toBool s = s == "c" || s == "C"

swapTurns :: Int -> Int
swapTurns t = 3-t   -- alternate between 1 and 2

-- convert 1, 2 to +1, -1
turnToColor :: Int -> Int
turnToColor 2 = -1
turnToColor _ = 1

--convert +1, -1 to 1, 2
colorToTurn :: Int -> Int
colorToTurn 1 = 1
colorToTurn _ = 2

createMessage :: String -> Tree Ck.CkNode -> NodeWrapper
createMessage s node = NodeWrapper {getNode = node, getLastMove = Nothing, 
                                    getJsonable = Jsonable (J.jsonMessage s)}

createUpdate :: String -> Tree Ck.CkNode -> Maybe Ck.CkMove -> NodeWrapper
createUpdate msg node mvMay = 
    NodeWrapper {getNode = node, 
                 getLastMove = mvMay, 
                 getJsonable = Jsonable $ J.jsonUpdate msg (rootLabel node) 
                                         (Ck.getPossibleMoves (rootLabel node))
                }
                 
createError :: String -> Tree Ck.CkNode -> NodeWrapper
createError s node = NodeWrapper {getNode = node, getLastMove = Nothing, 
                                  getJsonable = Jsonable (J.jsonError s)}
