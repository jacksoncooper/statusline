module Blocks.Battery
 ( batteryBlock
 )
where

import Control.Applicative (liftA2)
import System.IO (FilePath, readFile)

import Block
import Parse

batteryPath :: String -> FilePath
batteryPath battery =
     "/sys/class/power_supply/"
  ++ battery
  ++ "/capacity"

internalPath :: FilePath
internalPath = batteryPath "BAT0"

externalPath :: FilePath
externalPath = batteryPath "BAT1"

batteryBlock :: Block
batteryBlock =
  Block "battery" "combined" $  do
    maybeInternal <- runParser parseInteger <$> readFile internalPath
    maybeExternal <- runParser parseInteger <$> readFile externalPath

    let batteryPair = liftA2 (,) maybeInternal maybeExternal

    return $
      case batteryPair of
        Just (internal, external) ->
          "Main: " ++ show internal ++ " Extra: " ++ show external
        Nothing ->
          "[Failed to parse battery.]"
