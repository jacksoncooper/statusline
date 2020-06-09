module Emit
  ( emit
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.List (intercalate)
import System.IO
  ( BufferMode (LineBuffering)
  , hSetBuffering
  , stdout
  )

import Block
import Blocks.Battery
import Blocks.Brightness
import Blocks.Date
import Blocks.Audio

emit :: IO ()
emit =
  let
    doWrite = do
      dateText       <- fullText dateBlock
      batteryText    <- fullText batteryBlock
      brightnessText <- fullText brightnessBlock
      audioText      <- fullText audioBlock

      putStrLn . intercalate " | " $
        [dateText, batteryText, brightnessText, audioText]

      threadDelay (1 * 10 ^ 6)
  in
      hSetBuffering stdout LineBuffering >> forever doWrite
