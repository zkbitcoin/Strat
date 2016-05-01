{-# LANGUAGE MultiParamTypeClasses #-}
module CheckersTest where

import Checkers
import Test.Hspec
import Data.Tree
import Control.Lens
import Control.Lens.Setter
import StratTree.TreeNode

checkersTest =  
    describe "getPossibleMoves" $
        it "Gets the list of possible moves for a given color from a given position." $ do
            getPossibleMoves (rootLabel getStartNode) `shouldMatchList` [1419, 1519, 1520, 1620, 1621, 1721, 1722] --white moves
            getPossibleMoves blackFirstStartNode `shouldMatchList` [2823, 2824, 2924, 2925, 3025, 3026, 3126] --black moves
            
            getPossibleMoves (nodeFromGridW board01) `shouldMatchList` [510, 1721, 1722, 2832, 2833, 2823, 2824, 3934, 3935] 
            getPossibleMoves (nodeFromGridB board01) `shouldMatchList` [3732, 3733, 3025, 3026, 2024, 2025, 2015, 2016, 711, 712] 

            ---------------------------------------------------------------------------------------------------
-- Test helper functions
---------------------------------------------------------------------------------------------------            
blackFirstStartNode :: CkNode
blackFirstStartNode = rootLabel getStartNode & ckPosition.clr .~ (-1)

treeFromGridW :: [Int] -> Tree CkNode
treeFromGridW g = Node CkNode {_ckMove = -1, _ckValue = 0, _ckErrorValue = 0, 
    _ckPosition = CkPosition {_grid = g, _clr = 1, _fin = NotFinal}} []

treeFromGridB :: [Int] -> Tree CkNode
treeFromGridB g = Node CkNode {_ckMove = -1, _ckValue = 0, _ckErrorValue = 0, 
    _ckPosition = CkPosition {_grid = g, _clr = -1, _fin = NotFinal}} []
 
nodeFromGridW :: [Int] -> CkNode
nodeFromGridW g = rootLabel $ treeFromGridW g
 
nodeFromGridB :: [Int] -> CkNode
nodeFromGridB g = rootLabel $ treeFromGridB g 


---------------------------------------------------------------------------------------------------
-- Test board positions
---------------------------------------------------------------------------------------------------
board01 :: [Int]                   
board01 = [99, 99, 99, 99, 99, 01, 00, -02, 00, 99, 00, 00, 00, 00, 00, 00, 00, 01, 99, 00, -02, 00, 00,
           00, 00, 00, 00, 99, 02, 00, -1, 00, 00, 00, 00, 00, 99, -1, 00, 02, 00, 99, 99, 99, 99, 99]
           
{-                                   --  (41) (42) (43) (44) (45)    
                -1   00   02   00    --     37   38   39   40        
              00   00   00   00      --   32   33   34   35    (36)
                02   00   -1   00    --     28   29   30   31        
              00   00   00   00      --   23   24   25   26    (27)
                00   -2   00   00    --     19   20   21   22        
              00   00   00   01      --   14   15   16   17    (18)
                00   00   00   00    --     10   11   12   13        
              01   00   -2   00      --   05   06   07   08    (09)
                                     --  (00) (01) (02) (03) (04)    
--}


{-- 
board0n :: [Int]                    --  (41) (42) (43) (44) (45)    
board0n = [99, 99, 99, 99, 99, 01, 00, 02, 00, 99, 00, 00, 00, 00, 00, 00, 00, 01, 99, 00, -2, 00, 00,
           00, 00, 00, 00, 99, 02, 00, -1, 00, 00, 00, 00, 00, 99, -1, 00, 02, 00, 99, 99, 99, 99, 99]
           
                                     --  (41) (42) (43) (44) (45)    
                -1   00   02   00    --     37   38   39   40        
              00   00   00   00      --   32   33   34   35      (36)
                02   00   -1   00    --     28   29   30   31        
              00   00   00   00      --   23   24   25   26      (27)
                00   -2   00   00    --     19   20   21   22        
              00   00   00   01      --   14   15   16   17      (18)
                00   00   00   00    --     10   11   12   13        
              01   00   -2   00      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)       
-} 