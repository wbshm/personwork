import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Random

type Fork = TVar Bool
type StringBuffer = TChan String

philosopherNames :: [String]
philosopherNames = map show ([1..] :: [Int])

logHungry :: String -> StringBuffer -> STM ()
logHungry name buffer = writeTChan buffer $ "Philosopher" ++ name ++ " is hungry..."

logEating :: String -> StringBuffer -> STM ()
logEating name buffer = do
    let num = (read name::Int)
    writeTChan buffer $ "Philosopher" ++ name ++ " got forks "++show num ++" and "++ show (num `mod` 5 + 1)++" to eating..."

logDone :: String -> StringBuffer -> STM ()
logDone name buffer = writeTChan buffer $ "Philosopher" ++ name ++ " is done eating. Going back to thinking."

firstLogEntry :: StringBuffer -> STM String
firstLogEntry buffer = do
    empty <- isEmptyTChan buffer
    if empty then do
        retry
    else do
        readTChan buffer

takeForks :: Fork -> Fork -> STM ()
takeForks left right = do
    leftUsed <- readTVar left
    rightUsed <- readTVar right
    if leftUsed || rightUsed then
        retry
    else do
        writeTVar left True
        writeTVar right True

putForks :: Fork -> Fork -> STM ()
putForks left right = do writeTVar left False
                         writeTVar right False

philosopher :: String -> StringBuffer -> Fork -> Fork -> IO ()
philosopher name out left right = do
    atomically $ logHungry name out
    randomDelay
    atomically $ takeForks left right
    atomically $ logEating name out
    randomDelay
    atomically $ putForks left right
    atomically $ logDone name out
    randomDelay


randomDelay :: IO ()
randomDelay = do
    delay <- getStdRandom(randomR (1,5))
    threadDelay (delay * 1000000)

main :: IO ()
main = do
    let n = 5
    forks <- replicateM n $ newTVarIO False
    buffer <- newTChanIO
    forM_ [0 .. n - 1] $ \i -> do
        let left = forks !! i
        let right = forks !! ((i + 1) `mod` n)
        let name = philosopherNames !! i
        forkIO $ forever $ philosopher name buffer left right
    forever $ do
        str <- atomically $ firstLogEntry buffer
        putStrLn str

-- 主要思路： 多个临界资源，要么全部分配，要么一个都不分配，因此不会出现死锁的情形。
-- 即： 能同时拿到左右手的叉子才可以拿到叉子，不然拿不到叉子。
-- 缺点： 如果 1,3号轮流拿到叉子，有可能会导致2号一直都拿不到叉子。从而出现无限等待。
