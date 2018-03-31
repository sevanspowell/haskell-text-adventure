{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.RWS.Lazy (runRWS, liftIO)
import Data.List.Split (splitOn)
import Data.Foldable (for_)

import System.Console.Haskeline
import Sound.ALUT

import Data.GameEnvironment
import Data.GameState
import Game

defaultEnv :: GameEnvironment
defaultEnv = GameEnvironment "Player" False

runGame :: GameEnvironment -> GameState -> IO ()
runGame env state = runInputT defaultSettings go
  where
    go :: InputT IO ()
    go = do
        minput <- getInputLine "> "
        case minput of
            Nothing    -> pure ()
            Just input -> liftIO (loop state input)

    loop :: GameState -> String -> IO ()
    loop currentState input = do
        case runRWS (game (splitOn " " input)) env currentState of
            (_, newState, written) -> do
                for_ written putStrLn
                runGame env newState

data PlaybackOptions = PlaybackOptions
  { shouldLoop :: Bool
  }

playFile :: PlaybackOptions -> FilePath -> IO ()
playFile opt fp = do
  fileBuffer <- createBuffer $ File fp
  [source] <- genObjectNames 1
  queueBuffers source [fileBuffer]
  case (shouldLoop opt) of
    True  -> do 
      let
        loopVar = loopingMode source
      loopVar $= Looping
    False -> pure ()
  play [source]
    

-- Chris Double
-- https://bluishcoder.co.nz/articles/haskell/openal.html
playSound :: IO ()
playSound =
  withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ ->
  do
    (Just device) <- openDevice Nothing
    (Just context) <- createContext device []
    currentContext $= Just context
    playFile (PlaybackOptions { shouldLoop = True }) "data/bachfugue.wav"
    runGame defaultEnv initialGameState
    sleep 3
    closeDevice device
    return ()

main :: IO ()
-- main = runGame defaultEnv initialGameState
main = playSound

-- TODO Rewrite type of play sound to include a WriterT monad transform for logging
-- TODO Create safe wrapper for OpenAL methods that handles exceptions in style of FP course.
-- TODO Clean up code so logic is not so entangled