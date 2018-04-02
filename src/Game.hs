{-# LANGUAGE DeriveFunctor #-}

module Game where

import Data.GameEnvironment
import Data.GameState
import Data.GameItem
import Data.Coords

import Data.Map as M
import Data.Set as S
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Free
import Control.Monad.IO.Class
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get, modify, put)
import Control.Monad (forM_)

type Log = [String]

data PlaybackOptions = PlaybackOptions
  { shouldLoop :: Bool
  }

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

type Game' = FreeT Cmd

has' :: Monad m => GameItem -> Game' m Bool
has' item = liftF (Has item id)

pickup' :: Monad m => GameItem -> Game' m ()
pickup' item = liftF (Pickup item ())

say' :: Monad m => String -> Game' m ()
say' str = liftF (Say str ())

move' :: Monad m => Int -> Int -> Game' m ()
move' dx dy = liftF (Move dx dy ())

getPlayerName' :: Monad m => Game' m String
getPlayerName' = liftF (GetPlayerName id)

describeRoom' :: Monad m => Coords -> Game' m ()
describeRoom' Coords { x = 0, y = 0 } = say' "You are in a dark forest. You see a path to the north."
describeRoom' Coords { x = 0, y = 1 } = say' "You are in a clearing."
describeRoom' _ = say' "You are deep in the forest"

use' :: Monad m => PlayerName -> GameItem -> Game' m ()
use' _ Candle = say' "I don't know what you want me to do with that"
use' pName Matches = do
  hasCandle <- has' Candle
  if hasCandle
  then do
    say' "You light the candle."
    say' ("Congratulations, " ++ pName ++ "!")
    say' "You win!"
  else
    say' "You don't have anything to light." 

look' :: Monad m => Coords -> [GameItem] -> Game' m ()
look' loc itemList = do
  say' ("You are at " ++ prettyPrintCoords loc)
  describeRoom' loc
  forM_ itemList (\item -> say' (show item))

getPlayerLocation' :: Monad m => Game' m Coords
getPlayerLocation' = liftF (GetPlayerLocation id)

getItemsAtPlayerLocation' :: Monad m => Game' m [GameItem]
getItemsAtPlayerLocation' = liftF (GetItemsAtPlayerLocation id)

getState' :: Monad m => Game' m GameState
getState' = liftF (GetState id)

isDebugMode' :: Monad m => Game' m Bool
isDebugMode' = liftF (IsDebugMode id)

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
          put $ GameState { items     = newItems
                          , inventory = newInventory
                          , player    = (player s)
                          }
          liftIO $ putStrLn ("You now have the " ++ show item)
    _ -> liftIO $ putStrLn ("I don't see that item here.")

describeRoom :: Game ()
describeRoom = do
  s <- get
  case (player s) of
    Coords { x = 0, y = 0 } -> liftIO $ putStrLn ("You are in a dark forest. You see a path to the north.")
    Coords { x = 0, y = 1 } -> liftIO $ putStrLn ("You are in a clearing.") 
    _ -> liftIO $ putStrLn ("You are deep in the forest")

move :: Int -> Int -> Game ()
move dx dy = modify (\s -> GameState { items = (items s)
                                     , inventory = (inventory s)
                                     , player = updateCoords (player s)
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
    liftIO $ putStrLn ("You light the candle.")
    liftIO $ putStrLn ("Congratiulations, " ++ playerName env ++ "!")
    liftIO $ putStrLn ("You win!")
  else
    liftIO $ putStrLn ("You don't have anything to light.") 

-- Generate commands
game' :: Monad m => [String] -> Game' m ()
game' ["north"] = move' 0    1
game' ["south"] = move' 0    (-1)
game' ["east"] = move'  (-1) 0
game' ["west"] = move'  1    0
game' ["look"] = do
  pLoc <- getPlayerLocation'
  locItems <- getItemsAtPlayerLocation'
  look' pLoc locItems
game' ["debug"] = do
  isDebug <- isDebugMode'
  if isDebug
  then do
    s <- getState'
    say' (show s)
  else
    say' "Not running in debug mode"
game' ["use", item] =
  case readItem item of
    Nothing -> say' "I don't know what item you're referring to."
    Just gameItem -> do
      hasItem <- has' gameItem
      pName <- getPlayerName'
      if hasItem
        then use' pName gameItem
        else say' "You don't have that item"
game' ["take", item] =
  case readItem item of
    Nothing -> say' "I don't know what item you're referring to."
    Just gameItem -> pickup' gameItem
game' [] = pure ()
game' _ = say' "I don't understand"

-- game :: [String] -> Game ()
-- game ["look"] = do
--   s <- get
--   liftIO $ putStrLn (["You are at " ++ prettyPrintCoords (player s)])
--   describeRoom
--   forM_ (M.lookup (player s) (items s)) $ \items' ->
--     liftIO $ putStrLn (fmap (\item -> "You see the " ++ show item ++ ".") (S.toList items'))
-- game ["inventory"] = do
--   s <- get
--   liftIO $ putStrLn (fmap (\item -> "You have the " ++ show item ++ ".") (S.toList (inventory s)))
-- game ["north"] = move 0    (1)
-- game ["south"] = move 0    (-1)
-- game ["west"]  = move (-1) 0
-- game ["east"]  = move 1 0 
-- game ["take", item] =
--   case readItem item of
--     Nothing -> liftIO $ putStrLn ["I don't know what item you're referring to."]
--     Just gameItem -> pickUp gameItem
-- game ["use", item] =
--   case readItem item of
--     Nothing -> liftIO $ putStrLn ["I don't know what item you're referring to."]
--     Just gameItem -> do
--       hasItem <- has gameItem
--       if hasItem
--         then use gameItem
--         else liftIO $ putStrLn ["You don't have that item"]
-- game ["debug"] = do
--   env <- ask
--   if debugMode env
--     then do
--     s <- get
--     liftIO $ putStrLn [show s]
--   else liftIO $ putStrLn ["Not running in debug mode."]
-- game [] = pure ()
-- game _ = liftIO $ putStrLn ["I don't understand"]

type Game = ReaderT GameEnvironment (StateT GameState IO)

interpret :: Game' Game a -> Game a
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
        maybeItems = (M.lookup (player s) (items s))
        itemsList  = maybe [] (S.toList) maybeItems
      next itemsList
    morph (GetState next) = do
      s <- get
      next s
    morph (IsDebugMode next) = do
      env <- ask
      next (debugMode env)
    morph (PlayFile _ _ next) = do
      next
