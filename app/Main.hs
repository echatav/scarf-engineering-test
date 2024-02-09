{-# LANGUAGE MultiWayIf, NumericUnderscores #-}

module Main (main) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.WorkStealing
import Control.Monad.Loops
import Data.Char
import Data.List (isPrefixOf)
import System.Random
import Text.Read

data Task
  = BubbleBathOptimization BubbleBathParameters
  | SquirrelPatrol SquirrelParameters
  | UnicornWrangling UnicornParameters
  deriving Show

data BubbleBathParameters = BubbleBathParameters deriving Show
data SquirrelParameters = SquirrelParameters deriving Show
data UnicornParameters = UnicornParameters deriving Show

data Result = Result deriving Show

executorWork :: Task -> IO ()
executorWork task = do
  duration <- randomRIO (0, 1_000_000)
  putStrLn $ "running " ++ show task
  threadDelay duration
  let result = Result
  putStrLn $ "result " ++ show result

main :: IO ()
main = do

  putStrLn "Starting Executor"

  workersN <- untilJust $ do
    putStrLn "\nHow many concurrent workers? "
    readMaybe <$> getLine

  workers <- atomically $ newWorkers workersN

  workStealing workers executorWork $ do
    putStrLn "\nNew, View, Cancel tasks: "

    opt <- map toLower <$> getLine
    if
      | null opt -> return ()
      | opt `isPrefixOf` "new" -> do
          putStrLn "\nBubble Bath Optimization, Squirrel Patrol, or Unicorn Wrangling? "
          taskType <- map toLower <$> getLine
          if
            | null taskType -> putStrLn "Invalid Task"
            | taskType `isPrefixOf` "bubble bath optimization" ->
                atomically $ spawnWork workers
                  (BubbleBathOptimization BubbleBathParameters)
            | taskType `isPrefixOf` "squirrel patrol" ->
                atomically $ spawnWork workers
                  (SquirrelPatrol SquirrelParameters)
            | taskType `isPrefixOf` "unicorn wrangling" ->
                atomically $ spawnWork workers
                  (UnicornWrangling UnicornParameters)
            | otherwise -> putStrLn "Invalid Task"

      | opt `isPrefixOf` "view" -> do
          tasks <- atomically $ viewTasks workers
          print tasks

      | opt `isPrefixOf` "cancel" -> do
          task <- untilJust $ do
            putStrLn "Which task number? "
            readMaybe <$> getLine
          atomically $ cancelTask workers task

      | otherwise -> return ()
