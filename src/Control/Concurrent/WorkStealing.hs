{-# LANGUAGE LambdaCase, TupleSections #-}

module Control.Concurrent.WorkStealing
  ( Workers
  , Completion (..)
  , workStealing
  , newWorkers
  , spawnWork
  , viewTasks
  , cancelTask
  ) where

import Control.Applicative
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Foldable
import Data.Traversable
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

{- |
`Workers` is a static worker count and a dynamic task queue.
In the course of `workStealing`, the work in the task queue
is distributed to idle workers to perform concurrently
on a first in first out basis.
-}
data Workers w = Workers
  { workersCount :: Int
  , workerCancel :: IntMap (TMVar ())
    {- ^
    An `IntMap` of worker-ids to semaphores
    which signal a worker to cancel its in-progress work
    -}
  , workTasks :: TVar (IntMap (w, Completion))
    {- ^
    An `IntMap` of task-ids to work and `Completion` status
    -}
  , workQueue :: TQueue Int
    {- ^ task-ids that are queued for work -}
  }

{-| `Completion` status of a task -}
data Completion
  = Cancelled
  | Queued
  | InProgress Int -- ^ worker-id
  | Done
  deriving Show

{- |
`newWorkers` creates new `Workers` with the given count.
-}
newWorkers :: Int -> STM (Workers w)
newWorkers n = do
  emptyTasks <- newTVar IntMap.empty
  emptyQueue <- newTQueue
  emptyCancels <- do
    emptyCancelVars <- for [0 .. (n-1)] $ \worker ->
      (worker,) <$> newEmptyTMVar
    return $ IntMap.fromList emptyCancelVars
  return $ Workers
    { workersCount = n
    , workerCancel = emptyCancels
    , workTasks = emptyTasks
    , workQueue = emptyQueue
    }

{-| View all tasks and their current state of completion. -}
viewTasks :: Workers w -> STM (IntMap (w, Completion))
viewTasks = readTVar . workTasks

{-| Cancel a task. -}
cancelTask
  :: Workers w
  -> Int -- task-id
  -> STM ()
cancelTask workers task = do
  tasks <- readTVar (workTasks workers)
  for_ (IntMap.lookup task tasks) $ \case
    (w, Queued) -> do
      writeTVar (workTasks workers)
        (IntMap.insert task (w, Cancelled) tasks)
    (_, InProgress worker) -> do
      let cancelSemaphore = IntMap.lookup worker (workerCancel workers)
      for_ cancelSemaphore $ \sem -> writeTMVar sem ()
      -- signal worker to cancel in-progress work
      -- worker is also left in charge of marking task Cancelled
    _ -> return ()

{- |
`spawnWork` puts work which can feasibly be executed
concurrently with other work on the task queue.
In the course of `workStealing`,
both performing work and the base action 
may `spawnWork`.
-}
spawnWork :: Workers w -> w -> STM ()
spawnWork workers work = do
  tasks <- readTVar (workTasks workers)
  let task = IntMap.size tasks
  writeTQueue (workQueue workers) task
  let newTasks = IntMap.insert task (work, Queued) tasks
  writeTVar (workTasks workers) newTasks

{- |
`workStealing` schedules dynamically spawned work over
a static number of workers, distributing the work over idle workers,
from a first in first out task queue. Each idle worker looks at the queue,
and "steals" their work to perform concurrently.
While the queue is empty, all idle workers remain idle.
-}
workStealing
  :: Workers w
  {- |
  How all workers will _perform_ work. Workers should handle
  feasible, synchronous `IO` exceptions when performing work.
  If a worker throws an uncaught exception,
  `workStealing` will throw that exception.
  -}
  -> (w -> IO ())
  {- |
  The base action is run forever in parallel with the workers.
  -}
  -> IO ()
  -> IO ()
workStealing workers perform act = do
  let

    stealWork worker = do
      (cancelJob, doJob) <- atomically $ do
        task <- readTQueue (workQueue workers)
        tasks <- readTVar (workTasks workers)
        case IntMap.lookup task tasks of
          Just (work, Queued) -> do
            let updateTask completion =
                  IntMap.insert task (work, completion) tasks
                markTask completion =
                  writeTVar (workTasks workers) (updateTask completion)
            markTask (InProgress worker)
            let
              doJob = do
                perform work
                putStrLn $ "Task " ++ show task ++ " done by Worker " ++ show worker
                atomically $ markTask Done
              cancelJob = do
                let cancelSemaphore = IntMap.lookup worker (workerCancel workers)
                case cancelSemaphore of
                  Nothing -> runConcurrently empty -- thread delay forever
                  Just tmvar -> atomically (takeTMVar tmvar) -- block until task is cancelled
                putStrLn $ "Task " ++ show task ++ " cancelled by Worker " ++ show worker
                atomically $ markTask Cancelled
            return (cancelJob, doJob)
          _ -> return (return (), return ())
      race_ cancelJob doJob

    -- run (max 0 n) + 1 threads in parallel
    -- one for each worker and one for the base action
    runWorkers n
      | n <= 0 = forever act
      | otherwise = race_ (forever (stealWork (n-1))) (runWorkers (n-1))

  runWorkers (workersCount workers)
