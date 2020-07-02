module Blocks.Battery
 ( batteryBlock
 )
where

import Control.Applicative (liftA2)
import System.IO (FilePath, readFile)

import Block
import Parse

type Capacity = Int
type Status   = String

data Battery = Battery Capacity Status

batteryToString :: Battery -> String
batteryToString (Battery capacity status) =
  let
    statusSymbol =
      case status of
        "Charging"    -> " ▲" 
        "Discharging" -> " ▼"
        _             -> ""
  in
        show capacity
     ++ statusSymbol

batteryPath :: String -> String -> FilePath
batteryPath property battery =
     "/sys/class/power_supply/"
  ++ battery
  ++ "/"
  ++ property

capacityPath :: String -> FilePath
capacityPath = batteryPath "capacity"

statusPath :: String -> FilePath
statusPath = batteryPath "status"

batteryBlock :: Block
batteryBlock =
  Block "battery" "combined" $ do
    internalCapacity <-
          runParser parseInteger
      <$> readFile (capacityPath "BAT0")

    externalCapacity <-
          runParser parseInteger
      <$> readFile (capacityPath "BAT1")

    internalStatus <-
          runParser (parseUntil '\n')
      <$> readFile (statusPath "BAT0")

    externalStatus <-
          runParser (parseUntil '\n')
      <$> readFile (statusPath "BAT1")

    let internal = liftA2 Battery internalCapacity internalStatus
        external = liftA2 Battery externalCapacity externalStatus
        pair = liftA2 (,) internal external

    return $
      case pair of
        Just (internal, external) ->
             "Internal: "
          ++ batteryToString internal
          ++ " "
          ++ "External: "
          ++ batteryToString external
        Nothing ->
          "Failed to fetch battery."
