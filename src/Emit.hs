module Emit
  ( emit
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))

import Block
import Blocks.Date

-- TODO:
-- * Emit JSON as specified by 'swaybar-protocol'.
-- * Add another thread to listen for pause and continue signals.

emit :: IO ()
emit =
  let
    doWrite =
      getFullText dateBlock >>=
        \fullText ->
             putStrLn fullText
          >> threadDelay (1 * 10 ^ 6)
  in
        hSetBuffering stdout LineBuffering
     >> forever doWrite

