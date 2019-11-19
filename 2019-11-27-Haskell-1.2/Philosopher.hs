module Philosophers where
 
import Control.Monad
import Control.Concurrent

import Control.Concurrent.STM
import System.Random
 
-- TMVars are transactional references. They can only be used in transactional actions.
-- They are either empty or contain one value. Taking an empty reference fails and
-- putting a value in a full reference fails. A transactional action only succeeds
-- when all the component actions succeed, else it rolls back and retries until it
-- succeeds.
-- The Int is just for display purposes.
type Fork = TMVar Int

 -- newFork is Int  return IO Fork
newFork :: Int -> IO Fork
newFork i = newTMVarIO i
 
-- The basic transactional operations on forks
takeFork :: Fork -> STM Int
takeFork fork = takeTMVar fork
 
releaseFork :: Int -> Fork -> STM ()
releaseFork i fork = putTMVar fork i

type Name = String

runPhilosopher :: Name -> (Fork, Fork) -> IO ()
runPhilosopher name (left, right) = forever $ do
  putStrLn (name ++ " is hungry.")

  -- Run the transactional action atomically.
  -- The type system ensures this is the only way to run transactional actions.
  -- Only when the philosopher's left and right forks are taken up should he be allowed to eat.
  (leftNum, rightNum) <- atomically $ do
    leftNum <- takeFork left
    rightNum <- takeFork right
    return (leftNum, rightNum)

  putStrLn (name ++ " got forks " ++ show leftNum ++ " and " ++ show rightNum ++ " and is now eating.")
  randomDelay
  putStrLn (name ++ " is done eating. Going back to thinking.")

  atomically $ do
    releaseFork leftNum left
    releaseFork rightNum right

  randomDelay


randomDelay :: IO ()
randomDelay = do delay <- getStdRandom(randomR (1,10))
                 threadDelay (delay * 1000000)

philosophers :: [String]
philosophers = ["Philosopher1", "Philosopher2", "Philosopher3", "Philosopher4", "Philosopher5"]

main = do
  forks <- mapM newFork [1..5]
  let namedPhilosophers  = map runPhilosopher philosophers
      forkPairs          = zip forks (tail (forks ++ forks))
      philosophersWithForks = zipWith ($) namedPhilosophers forkPairs

  putStrLn "Running the philosophers. Press enter to quit."

  mapM_ forkIO philosophersWithForks

  -- All threads exit when the main thread exits.
  getLine

-- 主要思路： 多个临界资源，要么全部分配，要么一个都不分配，因此不会出现死锁的情形。
-- 即： 能同时拿到左右手的叉子才可以拿到叉子，不然拿不到叉子。
-- 缺点： 如果 1,3号轮流拿到叉子，有可能会导致2号一直都拿不到叉子。从而出现无限等待。
