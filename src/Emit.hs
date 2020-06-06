module Emit
  ( emit
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.List (intercalate)
import System.IO (stdout, hSetBuffering, BufferMode (LineBuffering))

import Block
import Blocks.Date
import Blocks.Battery
import Blocks.Brightness

emit :: IO ()
emit =
  let
    doWrite = do
      dateText       <- getFullText dateBlock
      batteryText    <- getFullText batteryBlock
      brightnessText <- getFullText brightnessBlock

      putStrLn . intercalate " | " $
        [dateText, batteryText, brightnessText]

      threadDelay (1 * 10 ^ 6)
  in
      hSetBuffering stdout LineBuffering >> forever doWrite
