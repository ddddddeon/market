module Main (main) where

import Yahoo (fetchTicker, getCloseOverTime, showTicks)

main :: IO ()
main = do
  closeOverTime <- getCloseOverTime . fetchTicker $ "GOOG"
  maybe (return ()) showTicks closeOverTime
