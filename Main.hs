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
import Logic

import Audio

defaultEnv :: GameEnvironment
defaultEnv = GameEnvironment "Player" False

run :: GameEnvironment -> GameState -> IO ()
run env state = runInputT defaultSettings go
  where
    go :: InputT IO ()
    go = do
        minput <- getInputLine "> "
        case minput of
            Nothing    -> pure ()
            Just input -> liftIO (game state input)

    game :: GameState -> String -> IO ()
    game currentState input = do
        (_, newState) <- runGame (interpret (logic (splitOn " " input))) env currentState
        case (shouldQuit newState) of
          True  -> pure ()
          False -> run env newState
    

-- Chris Double
-- https://bluishcoder.co.nz/articles/haskell/openal.html
main :: IO ()
main = withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ ->
  do
    (Just device) <- openDevice Nothing
    (Just context) <- createContext device []
    currentContext $= Just context
    -- playFile (PlaybackOptions { shouldLoop = True }) "data/bachfugue.wav"
    run defaultEnv initialGameState
    sleep 3
    closeDevice device
    return ()

-- TODO Rewrite type of play sound to include a WriterT monad transform for logging
-- TODO Create safe wrapper for OpenAL methods that handles exceptions in style of FP course.
