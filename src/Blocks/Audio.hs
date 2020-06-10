module Blocks.Audio
  ( audioBlock
  ) where

import Control.Applicative (liftA2, some)
import Data.Map.Lazy as Map (Map, fromList, lookup)
import Data.Maybe (fromMaybe)
import GHC.IO.Handle (hGetContents)
import System.Process
  ( StdStream (CreatePipe)
  , readCreateProcess
  , shell
  , std_out
  )

import Block
import Parse

type Contents = Map String String

data Channel = Channel Name Value
type Name = String
data Value = On Int | Off

amixerOutput :: IO String
amixerOutput =
  let process = shell "amixer get Master"
  in readCreateProcess process []

toContents :: String -> Maybe Contents
toContents =
  let
    pairParser = do
      parseWhitespace
      name <- parseUntil ':'
      parseCharacter ':'
      status <- parseUntil '\n'
      return (name, status)
  in
    runParser $
         parseThrough '\n'
      >> (fromList <$> some pairParser)

getChannel :: Contents -> Name -> Maybe Channel
getChannel contents name =
  let maybeValue = Map.lookup name contents >>= channelValue
   in Channel name <$> maybeValue

channelValue :: String -> Maybe Value
channelValue =
  let
    channelParser = do
      parseThrough '['
      value <- parseInteger
      parseThrough '['
      enabled <- parseUntil ']'

      return $
        if enabled == "on"
        then On value
        else Off
  in
    runParser channelParser

channelToString :: Channel -> String
channelToString channel =
  case channel of
    Channel _ (On value) -> show value
    _ -> "off"

channelsToString :: Channel -> Channel -> String
channelsToString left right =
     "Left: "
  ++ channelToString left
  ++ "Right: "
  ++ channelToString right

audioBlock :: Block
audioBlock =
  Block "audio" "internal" $
    amixerOutput >>= \output ->
      return . fromMaybe "Failed to fetch audio." $ do
        contents <- toContents output

        let maybeLeft     = getChannel contents "Front Left"
            maybeRight    = getChannel contents "Front Right"
            maybeChannels = liftA2 (,) maybeLeft maybeRight

        uncurry channelsToString <$> maybeChannels
