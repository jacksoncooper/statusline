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

-- Things to do:
-- 1. Emit JSON as specified by 'swaybar-protocol'.
-- 2. Add another thread to listen for pause and continue signals.
-- 3. Maybe use Text.ParserCombinators.ReadP in 'base' instad of rolling your
--    own set of parser combinators.

emit :: IO ()
emit =
  let
    doWrite = do
      dateText    <- getFullText dateBlock
      batteryText <- getFullText batteryBlock
      putStrLn . intercalate" | " $
        [dateText, batteryText]
      threadDelay (1 * 10 ^ 6)
  in
       hSetBuffering stdout LineBuffering
    >> forever doWrite

