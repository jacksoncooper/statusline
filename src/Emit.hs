module Emit
  ( emit
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)

import Block
import Blocks.Date

-- TODO: Emit JSON as specified by 'swaybar-protocol', and another thread to listen for pause
-- signals.

emit :: IO ()
emit = forever $
  getFullText dateBlock >>=
    \fullText ->
         putStrLn fullText
      >> threadDelay (1 * 10 ^ 6)

