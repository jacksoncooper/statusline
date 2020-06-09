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

getStatus :: IO String
getStatus =
  let process = shell "amixer get Master"
  in readCreateProcess process []

statusToMap :: String -> Maybe (Map String String)
statusToMap =
  let
    pairParser = do
      parseWhitespace
      name <- parseUntil ':'
      parseCharacter ':'
      status <- parseUntil '\n'
      return (name, status)
  in
    runParser $
         parseUntil '\n'
      >> (fromList <$> some pairParser)

channelValue :: String -> Maybe (Bool, Int)
channelValue =
  let
    channelParser = do
      parseUntil '['
      parseCharacter '['
      value <- parseInteger
      parseUntil '['
      parseCharacter '['
      enabled <- parseUntil ']'
      return (enabled == "on", value) 
  in
    runParser channelParser

audioBlock :: Block
audioBlock =
  Block "audio" "internal" $
    let
      audioStatus =
        getStatus >>= \status ->
          return $
            let
              statuses = statusToMap status
              maybeLeft =
                    statuses
                >>= Map.lookup "Front Left"
                >>= channelValue
              maybeRight =
                    statuses
                >>= Map.lookup "Front Right"
                >>= channelValue
              channelToString channel =
                case channel of
                  (True, value) -> show value
                  _ -> "off"
              channelsToString (left, right) =
                  "Left: "
                ++ channelToString left
                ++ " Right: "
                ++ channelToString right
            in
              channelsToString <$> liftA2 (,) maybeLeft maybeRight
    in
      audioStatus >>=
        \audioText ->
          return $
            case audioText of
              Just text -> text
              Nothing   -> "[Failed to fetch audio.]"
