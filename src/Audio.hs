module Audio where

import Sound.ALUT

-- instance MonadAudio IO where
--   playFile = playFile

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
