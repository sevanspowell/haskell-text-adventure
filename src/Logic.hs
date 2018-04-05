{-# LANGUAGE DeriveFunctor #-}

module Logic where

import Data.GameEnvironment
import Data.GameState
import Data.GameItem
import Data.Coords

import Audio (PlaybackOptions(..))

import Control.Monad.Trans.Free (FreeT, liftF)
import Control.Monad (forM_)

data Cmd ret
  = Say String ret
  | Move Int Int ret
  | Pickup GameItem ret
  | Has GameItem (Bool -> ret)
  | GetPlayerName (String -> ret)
  | GetPlayerLocation (Coords -> ret)
  | GetItemsAtPlayerLocation ([GameItem] -> ret)
  | GetState (GameState -> ret)
  | IsDebugMode (Bool -> ret)
  | PlayFile PlaybackOptions FilePath ret
  deriving Functor

type Program = FreeT Cmd

has' :: Monad m => GameItem -> Program m Bool
has' item = liftF (Has item id)

pickup' :: Monad m => GameItem -> Program m ()
pickup' item = liftF (Pickup item ())

say' :: Monad m => String -> Program m ()
say' str = liftF (Say str ())

move' :: Monad m => Int -> Int -> Program m ()
move' dx dy = liftF (Move dx dy ())

getPlayerName' :: Monad m => Program m String
getPlayerName' = liftF (GetPlayerName id)

playFile' :: Monad m => PlaybackOptions -> FilePath -> Program m ()
playFile' opt file = liftF (PlayFile opt file ())

getPlayerLocation' :: Monad m => Program m Coords
getPlayerLocation' = liftF (GetPlayerLocation id)

getItemsAtPlayerLocation' :: Monad m => Program m [GameItem]
getItemsAtPlayerLocation' = liftF (GetItemsAtPlayerLocation id)

getState' :: Monad m => Program m GameState
getState' = liftF (GetState id)

isDebugMode' :: Monad m => Program m Bool
isDebugMode' = liftF (IsDebugMode id)

describeRoom' :: Monad m => Coords -> Program m ()
describeRoom' Coords { x = 0, y = 0 } = say' "You are in a dark forest. You see a path to the north."
describeRoom' Coords { x = 0, y = 1 } = say' "You are in a clearing."
describeRoom' _ = say' "You are deep in the forest"

use' :: Monad m => PlayerName -> GameItem -> Program m ()
use' _ Candle = say' "I don't know what you want me to do with that"
use' pName Matches = do
  hasCandle <- has' Candle
  if hasCandle
  then do
    say' "You light the candle."
    say' ("Congratulations, " ++ pName ++ "!")
    say' "You win!"
    playFile' (PlaybackOptions { shouldLoop = False }) "data/bachfugue.wav"
  else
    say' "You don't have anything to light." 

look' :: Monad m => Coords -> [GameItem] -> Program m ()
look' loc itemList = do
  say' ("You are at " ++ prettyPrintCoords loc)
  describeRoom' loc
  forM_ itemList (\item -> say' ("You see the " ++ show item ++ "."))

logic :: Monad m => [String] -> Program m ()
logic ["north"] = move' 0    1
logic ["south"] = move' 0    (-1)
logic ["east"] = move'  (-1) 0
logic ["west"] = move'  1    0
logic ["look"] = do
  pLoc <- getPlayerLocation'
  locItems <- getItemsAtPlayerLocation'
  look' pLoc locItems
logic ["debug"] = do
  isDebug <- isDebugMode'
  if isDebug
  then do
    s <- getState'
    say' (show s)
  else
    say' "Not running in debug mode"
logic ["use", item] =
  case readItem item of
    Nothing -> say' "I don't know what item you're referring to."
    Just gameItem -> do
      hasItem <- has' gameItem
      pName <- getPlayerName'
      if hasItem
        then use' pName gameItem
        else say' "You don't have that item"
logic ["take", item] =
  case readItem item of
    Nothing -> say' "I don't know what item you're referring to."
    Just gameItem -> pickup' gameItem
logic [] = pure ()
logic _ = say' "I don't understand"
