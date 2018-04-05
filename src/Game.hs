module Game where

import Data.GameEnvironment
import Data.GameState
import Data.GameItem
import Data.Coords

import Logic

import Data.Map as M
import Data.Set as S
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.Trans.Free
import Control.Monad.IO.Class
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get, modify, put)
import Control.Monad (forM_)

import System.Console.Haskeline

import Audio (PlaybackOptions(..), playFile)

-- Implementations for Game transformer stack

type Game = ReaderT GameEnvironment (StateT GameState IO)

runGame :: Game a -> GameEnvironment -> GameState -> IO (a, GameState)
runGame game env state = (runStateT (runReaderT game env) state)

-- runGame (interpret (game' ["debug"])) defaultEnv initialGameState

has :: GameItem -> Game Bool
has item = do
  s <- get
  pure $ item `S.member` (inventory s)

pickUp :: GameItem -> Game ()
pickUp item = do
  s <- get
  case (player s) `M.lookup` (items s) of
    Just itemsHere
      | item `S.member` itemsHere -> do
          let newItems = M.update (Just . S.delete item) (player s) (items s)
              newInventory = S.insert item (inventory s)
          put $ GameState { items      = newItems
                          , inventory  = newInventory
                          , player     = (player s)
                          , shouldQuit = (shouldQuit s)
                          }
          liftIO $ putStrLn ("You now have the " ++ show item)
    _ -> liftIO $ putStrLn "I don't see that item here."

describeRoom :: Game ()
describeRoom = do
  s <- get
  case (player s) of
    Coords { x = 0, y = 0 } -> liftIO $ putStrLn "You are in a dark forest. You see a path to the north."
    Coords { x = 0, y = 1 } -> liftIO $ putStrLn "You are in a clearing."
    _ -> liftIO $ putStrLn "You are deep in the forest"

move :: Int -> Int -> Game ()
move dx dy = modify (\s -> GameState { items      = (items s)
                                     , inventory  = (inventory s)
                                     , player     = updateCoords (player s)
                                     , shouldQuit = (shouldQuit s)
                                     })
  where
  updateCoords :: Coords -> Coords
  updateCoords (Coords px py) = coords (px + dx) (py + dy)

use :: GameItem -> Game ()
use Candle = liftIO $ putStrLn ("I don't know what you want me to do with that.")
use Matches = do
  hasCandle <- has Candle
  if hasCandle
  then do
    env <- ask
    liftIO $ putStrLn "You light the candle."
    liftIO $ putStrLn ("Congratulations, " ++ playerName env ++ "!")
    liftIO $ putStrLn "You win!"
  else
    liftIO $ putStrLn "You don't have anything to light."

quit :: Game ()
quit = modify (\s -> GameState { items      = (items s)
                               , inventory  = (inventory s)
                               , player     = (player s)
                               , shouldQuit = True
                               })

confirm :: String -> Game Bool
confirm msg = do
  liftIO $ putStrLn msg
  liftIO $ runInputT defaultSettings getConfirmation
    where
      getConfirmation :: InputT IO Bool
      getConfirmation = do
        minput <- getInputLine ""
        case minput of
          Just ("yes") -> pure (True)
          Just ("y")   -> pure (True)
          _            -> pure (False)

-- Interpret commands

interpret :: Program Game a -> Game a
interpret = iterT morph
  where
    morph :: Cmd (Game a) -> Game a
    morph (Say s next) = do
      liftIO $ putStrLn s
      next
    morph (Has item next) = do
      hasItem <- has item
      next $ hasItem
    morph (Move dx dy next) = do
      move dx dy
      next
    morph (Pickup item next) = do
      pickUp item
      next
    morph (GetPlayerName next) = do
      env <- ask
      next (playerName env)
    morph (GetPlayerLocation next) = do
      s <- get
      next (player s)
    morph (GetItemsAtPlayerLocation next) = do
      s <- get
      let
        itemsList  = maybe [] (S.toList) (M.lookup (player s) (items s))
      next itemsList
    morph (GetState next) = do
      s <- get
      next s
    morph (IsDebugMode next) = do
      env <- ask
      next (debugMode env)
    morph (PlayFile opt file next) = do
      liftIO $ playFile opt file
      next
    morph (Quit next) = do
      quit
      next
    morph (Confirm msg next) = do
      result <- confirm msg
      next result
