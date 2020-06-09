module Blocks.Audio
  ( audioBlock
  ) where

import Control.Applicative (liftA2, some)
import Data.Map.Lazy as Map (Map, fromList, lookup)
import GHC.IO.Handle (hGetContents)
import System.Process
  ( StdStream (CreatePipe)
  , readCreateProcess
  , shell
  , std_out
  )

import Block
import Parse

-- A semi-closed handle becomes closed once the entire contents of the handle
-- has been read. (System.IO)

getStatus :: IO String
getStatus =
  let process = shell "amixer get Master"
  in readCreateProcess process []

statusToMap :: String -> Maybe (Map String String)
statusToMap =
  let
    pairParser = do
      parseWhitespace
      name  <- parseUntil ':'
      parseCharacter ':'
      status <- parseUntil '\n'
      return (name, status)
  in
    runParser $
         parseUntil '\n'
      >> (fromList <$> some pairParser)

getPercentage :: String -> Maybe Int
getPercentage =
  let
    percentageParser = do
      parseUntil '['
      parseCharacter '['
      parseInteger
  in
    runParser percentageParser

audioBlock :: Block
audioBlock =
  Block "audio" "internal" $
    let
      audioStatus =
        getStatus >>= \status ->
          return $
            let
              statuses =
                statusToMap status
              maybeLeft =
                statuses >>= Map.lookup "Front Left" >>= getPercentage
              maybeRight =
                statuses >>= Map.lookup "Front Right" >>= getPercentage
              toReadable (left, right) =
                  "Left: "
                ++ show left
                ++ "% Right: "
                ++ show right
                ++ "%"
            in
              toReadable <$> liftA2 (,) maybeLeft maybeRight
    in
      audioStatus >>=
        \audioText ->
          return $
            case audioText of
              Just text -> text
              Nothing   -> "[Failed to fetch audio.]"
