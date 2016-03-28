{-# LANGUAGE MultiParamTypeClasses #-}
module StratTree.TreeNode (TreeNode (..), PositionNode (..), FinalState (..), flipColor, 
       MoveScore (..), Result (..), Env (..)) where
 
-------------------------------------------------------------
-- Data types
-------------------------------------------------------------
class TreeNode t where
    getMove :: t -> Int
    getValue :: t -> Int
 
class TreeNode n => PositionNode n where
    newNode :: n -> Int -> n
    color :: n -> Int
    evaluate :: n -> Int    
    possibleMoves :: n -> [Int]
    final :: n -> FinalState
    
data FinalState = WWins | BWins | Draw | NotFinal deriving (Enum, Show, Eq)

data Env = Env 
    {_depth :: Int, _errorDepth :: Int, _equivThreshold :: Int, _errorEquivThreshold :: Int,
     _p1Comp :: Bool, _p2Comp :: Bool } deriving (Show)

data MoveScore = MoveScore {_move :: Int, _score :: Int} deriving (Show, Eq)

data Result = Result {_moveChoices :: [Int], _followingMoves :: [Int], _moveScores ::[MoveScore]} 
                deriving(Show, Eq)

-------------------------------------------------------------------------------
flipColor :: Int -> Int
flipColor = negate      --alternate 1 / -1
