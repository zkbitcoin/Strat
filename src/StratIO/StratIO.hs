module StratIO.StratIO (resolveRandom) where

import StratTree.TreeNode
import System.Random
import System.IO

resolveRandom :: Maybe [MoveScore] -> IO (Maybe Int)
resolveRandom Nothing = return Nothing
resolveRandom (Just []) = return Nothing
resolveRandom (Just xs) = do
    --let ints = fmap (\x -> _move x) xs
    let ints = fmap _move xs
    r <- getStdRandom $ randomR (1, length ints)
    let n = ints !! (r-1)
    --putStrLn ("selecting: " ++ show n ++ " from: " ++ show xs)
    return (Just n) 
 